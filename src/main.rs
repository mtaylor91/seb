use http_body_util::{BodyExt, combinators::BoxBody, StreamBody};
use hyper::{body::{Bytes, Frame, Incoming}, Request, Response, StatusCode};
use hyper_util::rt::{TokioExecutor, TokioIo};
use std::collections::HashMap;
use std::future::Future;
use std::pin::Pin;
use std::sync::{Arc, Mutex};
use std::vec::IntoIter;
use tokio::signal::unix::{signal, SignalKind};
use tokio::sync::broadcast::{self, error::SendError};
use tokio_stream::{Iter, StreamExt};
use tokio_stream::wrappers::{BroadcastStream, errors::BroadcastStreamRecvError};

pub enum Error {
    HyperHttp(hyper::http::Error),
    TooFarBehind(u64),
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::HyperHttp(e) => write!(f, "Hyper HTTP error: {:?}", e),
            Error::TooFarBehind(n) => write!(f, "Too far behind: {}", n),
        }
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::HyperHttp(e) => write!(f, "Hyper HTTP error: {}", e),
            Error::TooFarBehind(n) => write!(f, "Too far behind: {}", n),
        }
    }
}

impl std::error::Error for Error {}


pub struct Mailbox {
    messages: Vec<Bytes>,
    tx: broadcast::Sender<Bytes>,
}

impl Mailbox {
    pub fn new() -> Self {
        let (tx, _rx) = broadcast::channel(10);
        Mailbox { messages: Vec::new(), tx }
    }

    pub fn send(&mut self, data: Bytes) -> Result<usize, SendError<Bytes>> {
        self.messages.push(data.clone());
        self.tx.send(data)
    }

    pub fn replay(&self) -> Iter<IntoIter<Bytes>> {
        tokio_stream::iter(self.messages.clone())
    }

    pub fn stream(&self) -> BroadcastStream<Bytes> {
        BroadcastStream::new(self.tx.subscribe())
    }
}


type ServiceResponse = Response<BoxBody<Bytes, Error>>;
type ServiceResult = Result<ServiceResponse, Error>;
type ServiceResultFuture<'a> = Pin<Box<dyn Future<Output = ServiceResult> + Send + 'a>>;


pub struct ServiceHandler {
    mailboxes: Arc<Mutex<HashMap<String, Mailbox>>>,
}

impl Clone for ServiceHandler {
    fn clone(&self) -> Self {
        ServiceHandler {
            mailboxes: self.mailboxes.clone(),
        }
    }
}

impl ServiceHandler {
    fn handle_get(&self, req: Request<Incoming>) -> ServiceResultFuture {
        let path = req.uri().path().to_string();
        let mailboxes = self.mailboxes.lock().unwrap();

        match mailboxes.get(&path) {
            Some(mailbox) => {
                let replay_stream = mailbox.replay()
                    .map(|x| Ok(Frame::data(x)));
                let tx_stream = mailbox.stream()
                    .map(|x| match x {
                        Ok(data) => Ok(Frame::data(data)),
                        Err(BroadcastStreamRecvError::Lagged(n)) =>
                            Err(Error::TooFarBehind(n)),
                    });
                let stream = replay_stream.chain(tx_stream).take(10);
                let body = BoxBody::new(StreamBody::new(stream));
                let result = Response::builder()
                    .status(StatusCode::OK)
                    .body(body);
                match result {
                    Ok(response) => Box::pin(async { Ok(response) }),
                    Err(e) => Box::pin(async { Err(Error::HyperHttp(e)) }),
                }
            },
            None => {
                let stream = futures::stream::once(async {
                    Ok(Frame::data(Bytes::from_static(b"Mailbox not found")))
                });
                let body = BoxBody::new(StreamBody::new(stream));
                let result = Response::builder()
                    .status(StatusCode::NOT_FOUND)
                    .body(body);
                match result {
                    Ok(response) => Box::pin(async { Ok(response) }),
                    Err(e) => Box::pin(async { Err(Error::HyperHttp(e)) }),
                }
            }
        }
    }

    fn handle_post(&self, req: Request<Incoming>) -> ServiceResultFuture {
        let path = req.uri().path().to_string();

        {
            let mut mailboxes = self.mailboxes.lock().unwrap();
            if !mailboxes.contains_key(&path) {
                mailboxes.insert(path.clone(), Mailbox::new());
            }
        }

        Box::pin(async move {
            let mut incoming = req.into_body();

            loop {
                let frame = incoming.frame().await;
                match frame {
                    Some(Ok(chunk)) => {
                        if let Some(data) = chunk.data_ref() {
                            // Send the message
                            let mut mailboxes = self.mailboxes.lock().unwrap();
                            let mailbox = mailboxes.get_mut(&path).unwrap();
                            if mailbox.send(data.clone()).is_err() {
                                eprintln!("Failed to broadcast message");
                                break;
                            }
                        }
                    },
                    Some(Err(e)) => {
                        eprintln!("Error reading request body: {:?}", e);
                        break;
                    },
                    None => {
                        eprintln!("Connection closed");
                        break;
                    },
                }
            }

            let stream_body = StreamBody::new(futures::stream::once(async {
                Ok(Frame::data(Bytes::from_static(b"Message received")))
            }));

            let result = Response::builder()
                .status(StatusCode::OK)
                .body(BoxBody::new(stream_body));

            match result {
                Ok(response) => Ok(response),
                Err(e) => Err(Error::HyperHttp(e)),
            }
        })
    }
}


pub struct ServiceHandlerWrapper {
    inner: ServiceHandler,
}

impl Clone for ServiceHandlerWrapper {
    fn clone(&self) -> Self {
        ServiceHandlerWrapper {
            inner: self.inner.clone(),
        }
    }
}

impl hyper::service::Service<Request<Incoming>> for ServiceHandlerWrapper {
    type Error = Error;
    type Future = ServiceResultFuture<'static>;
    type Response = ServiceResponse;

    fn call(&self, req: Request<hyper::body::Incoming>) -> Self::Future {
        let handler = self.inner.clone();
        Box::pin(async move {
            match req.method() {
                &hyper::Method::GET => handler.handle_get(req).await,
                &hyper::Method::POST => handler.handle_post(req).await,
                _ => {
                    let stream = futures::stream::once(async {
                        Ok(Frame::data(Bytes::from_static(b"Method not allowed")))
                    });
                    let body = BoxBody::new(StreamBody::new(stream));
                    let result = Response::builder()
                        .status(StatusCode::METHOD_NOT_ALLOWED)
                        .body(body);
                    match result {
                        Ok(response) => Ok(response),
                        Err(e) => Err(Error::HyperHttp(e)),
                    }
                }
            }
        })
    }
}


async fn shutdown_signal() -> () {
    let mut sigint = signal(SignalKind::interrupt())
        .expect("failed to install SIGINT signal handler");
    let mut sigterm = signal(SignalKind::terminate())
        .expect("failed to install SIGTERM signal handler");

    tokio::select! {
        _ = sigint.recv() => {
            eprintln!("Received SIGINT");
        },
        _ = sigterm.recv() => {
            eprintln!("Received SIGTERM");
        }
    }
}


#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let addr = std::net::SocketAddr::from(([0, 0, 0, 0], 3000));

    let executor = TokioExecutor::default();
    let listener = tokio::net::TcpListener::bind(&addr).await?;
    let http = hyper_util::server::conn::auto::Builder::new(executor);
    let mailboxes = Arc::new(Mutex::new(HashMap::new()));
    let service = ServiceHandlerWrapper {
        inner: ServiceHandler { mailboxes }
    };

    let graceful = hyper_util::server::graceful::GracefulShutdown::new();
    let mut signal = std::pin::pin!(shutdown_signal());

    loop {
        tokio::select! {
            Ok((stream, addr)) = listener.accept() => {
                eprintln!("Accepted connection from: {}", addr);
                let io = TokioIo::new(stream);
                let conn = http.serve_connection(io, service.clone());
                // watch this connection
                let fut = graceful.watch(conn.into_owned());
                tokio::spawn(async move {
                    if let Err(e) = fut.await {
                        eprintln!("server connection error: {}", e);
                    }
                });
            },
            _ = &mut signal => {
                // Shutdown signal received
                eprintln!("Shutting down...");
                break;
            }
        }
    }

    tokio::select! {
        _ = graceful.shutdown() => {
            eprintln!("Graceful shutdown complete");
        }
    }

    Ok(())
}
