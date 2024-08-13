use http_body_util::{BodyExt, combinators::BoxBody, StreamBody};
use hyper::{body::{Bytes, Frame}, Request, Response, StatusCode};
use hyper_util::rt::{TokioExecutor, TokioIo};
use std::future::Future;
use std::pin::Pin;
use tokio::signal::unix::{signal, SignalKind};
use tokio::sync::broadcast;
use tokio_stream::{StreamExt, wrappers::BroadcastStream};

pub enum Error {
    Io(std::io::Error),
    Hyper(hyper::Error),
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::Io(e) => write!(f, "IO error: {:?}", e),
            Error::Hyper(e) => write!(f, "Hyper error: {:?}", e),
        }
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::Io(e) => write!(f, "IO error: {}", e),
            Error::Hyper(e) => write!(f, "Hyper error: {}", e),
        }
    }
}

impl std::error::Error for Error {}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Error::Io(e)
    }
}

impl From<hyper::Error> for Error {
    fn from(e: hyper::Error) -> Self {
        Error::Hyper(e)
    }
}

type ServiceResponse = Response<BoxBody<Bytes, hyper::Error>>;
type ServiceResult = Result<ServiceResponse, Error>;
type ServiceResultFuture = Pin<Box<dyn Future<Output = ServiceResult> + Send>>;

pub struct ServiceHandler {
    tx: broadcast::Sender<Bytes>,
}

impl Clone for ServiceHandler {
    fn clone(&self) -> Self {
        ServiceHandler {
            tx: self.tx.clone(),
        }
    }
}

impl hyper::service::Service<Request<hyper::body::Incoming>> for ServiceHandler {
    type Error = Error;
    type Future = ServiceResultFuture;
    type Response = ServiceResponse;

    fn call(&self, req: Request<hyper::body::Incoming>) -> Self::Future {
        match req.method() {
            &hyper::Method::POST => {
                self.handle_post(req)
            },
            &hyper::Method::GET => {
                self.handle_get()
            },
            _ => {
                let stream = futures::stream::once(async {
                    Ok(Frame::data(Bytes::from_static(b"Method not allowed")))
                });
                let body = StreamBody::new(stream);
                Box::pin(async {
                    Ok(Response::builder()
                        .status(StatusCode::METHOD_NOT_ALLOWED)
                        .body(BoxBody::new(body))
                        .expect("Failed to construct Response"))
                })
            }
        }
    }
}

impl ServiceHandler {
    fn handle_get(&self) -> ServiceResultFuture {
        let tx = self.tx.clone();
        Box::pin(async move {
            let body_stream = BroadcastStream::new(tx.subscribe())
                .map(|x| {
                    eprintln!("Forwarding message");
                    Ok(Frame::data(x.unwrap()))
                });

            let stream_body = StreamBody::new(body_stream);
            let boxed_body = BoxBody::new(stream_body);

            Ok(Response::builder()
                .status(StatusCode::OK)
                .body(boxed_body)
                .expect("Failed to construct Response"))
        })
    }

    fn handle_post(&self, req: Request<hyper::body::Incoming>) -> ServiceResultFuture {
        let tx = self.tx.clone();
        Box::pin(async move {
            let mut incoming = req.into_body();

            loop {
                let frame = incoming.frame().await;
                match frame {
                    Some(Ok(chunk)) => {
                        if let Some(data) = chunk.data_ref() {
                            if tx.send(data.clone()).is_err() {
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

            Ok(Response::builder()
                .status(StatusCode::OK)
                .body(BoxBody::new(stream_body))
                .expect("Failed to construct Response"))
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
    let (tx, _rx) = broadcast::channel(10);
    let service = ServiceHandler { tx };

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
