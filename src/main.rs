use futures::stream;
use http_body_util::{combinators::BoxBody, StreamBody};
use hyper::{body::{Bytes, Frame}, Request, Response, StatusCode};
use hyper_util::rt::TokioIo;
use std::future::Future;
use std::pin::Pin;

pub struct ServiceHandler;

impl Clone for ServiceHandler {
    fn clone(&self) -> Self {
        ServiceHandler
    }
}

impl hyper::service::Service<Request<hyper::body::Incoming>> for ServiceHandler {
    type Response = Response<BoxBody<Bytes, hyper::Error>>;
    type Error = hyper::Error;
    type Future = Pin<Box<dyn Future<Output = Result<Self::Response, Self::Error>> + Send>>;

    fn call(&self, _req: Request<hyper::body::Incoming>) -> Self::Future {
        Box::pin(async move {
            let body_stream = stream::once(async {
                Ok::<_, hyper::Error>(Frame::data(Bytes::from_static(b"Hello, World!")))
            });

            let stream_body = StreamBody::new(body_stream);
            let boxed_body = BoxBody::new(stream_body);

            Ok(Response::builder()
                .status(StatusCode::OK)
                .body(boxed_body)
                .expect("Failed to construct Response"))
        })
    }
}


async fn shutdown_signal() -> () {
    // Wait for the CTRL+C signal
    tokio::signal::ctrl_c()
        .await
        .expect("failed to install CTRL+C signal handler");
}


#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let addr = std::net::SocketAddr::from(([0, 0, 0, 0], 3000));

    let listener = tokio::net::TcpListener::bind(&addr).await?;
    let http = hyper::server::conn::http1::Builder::new();
    let service = ServiceHandler;

    let graceful = hyper_util::server::graceful::GracefulShutdown::new();
    let mut signal = std::pin::pin!(shutdown_signal());

    loop {
        tokio::select! {
            Ok((stream, addr)) = listener.accept() => {
                eprintln!("Accepted connection from: {}", addr);
                let io = TokioIo::new(stream);
                let conn = http.serve_connection(io, service.clone());
                // watch this connection
                let fut = graceful.watch(conn);
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
