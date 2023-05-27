use axum::{extract::State, routing::post, Json, Router};
use llm;
use serde::{Deserialize, Serialize};
use std::net::{SocketAddr, ToSocketAddrs};
use std::path::{Path, PathBuf};
use tokio;

use crate::model::Model;
use crate::prompts::Prompt;
use std::error::Error;

pub async fn load_server<M: llm::KnownModel + 'static>(model_path: &str, addr: &String) {
    let model = Model::new::<M>(model_path).unwrap();
    let server = Server::new(addr, model);
    server.start().await;
}

#[derive(Debug, Serialize, Deserialize)]
enum PromptType {
    FIX,
    GENERATE,
    EMACS,
}

#[derive(Debug, Serialize, Deserialize)]
struct Request {
    prompt_type: PromptType,
    data: String,
}

#[derive(Debug, Serialize, Deserialize)]
struct Tokens {
    data: String,
}

#[axum::debug_handler]
async fn sse_prompt(State(model): State<Model>, message: Json<Request>) -> Json<Tokens> {
    let prompt = Prompt::new("src/prompts/");

    let gen = match &message.prompt_type {
        PromptType::FIX => prompt.fix(&message.data),
        PromptType::GENERATE => prompt.generate(&message.data),
        PromptType::EMACS => prompt.emacs(&message.data),
    };

    let session = model.run_session(&gen);

    let response = Tokens { data: session };

    Json(response)
}

async fn shutdown_signal() {
    let ctrl_c = async {
        tokio::signal::ctrl_c()
            .await
            .expect("failed to install Ctrl+C handler");
    };

    #[cfg(unix)]
    let terminate = async {
        tokio::signal::unix::signal(tokio::signal::unix::SignalKind::terminate())
            .expect("failed to install signal handler")
            .recv()
            .await;
    };

    #[cfg(not(unix))]
    let terminate = std::future::pending::<()>();

    tokio::select! {
        _ = ctrl_c => {},
        _ = terminate => {},
    }

    println!("signal received, starting graceful shutdown");
}

pub struct Server {
    addr: SocketAddr,
    model: Model,
}

impl Server {
    pub fn new(_addr: &String, model: Model) -> Self {
        match string_to_socketaddr(_addr) {
            Ok(res) => Self { addr: res, model },
            Err(err) => panic!("Error: {}", err),
        }
    }

    pub async fn start(&self) {
        let app = Router::new()
            .route("/", post(sse_prompt))
            .with_state(self.model.clone());

        let server = axum::Server::bind(&self.addr)
            .serve(app.into_make_service())
            .with_graceful_shutdown(shutdown_signal());

        println!("Server running on http://{}", &self.addr);

        if let Err(err) = server.await {
            eprintln!("Server error: {}", err);
        }
    }
}

fn string_to_socketaddr(address_string: &str) -> Result<SocketAddr, Box<dyn std::error::Error>> {
    // Use the to_socket_addrs() method to parse the address string
    let socket_addrs = address_string.to_socket_addrs()?;

    // Extract the first socket address from the iterator
    if let Some(socket_addr) = socket_addrs.into_iter().next() {
        Ok(socket_addr)
    } else {
        // Handle the case when no socket addresses are found
        Err("Unable to resolve socket address".into())
    }
}
