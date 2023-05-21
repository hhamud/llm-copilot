use axum::{extract::State, routing::post, Json, Router};
use serde::{Deserialize, Serialize};
use std::net::SocketAddr;
use std::path::PathBuf;
use tokio;

use crate::model::Model;
use crate::prompts::Prompt;

#[derive(Debug, Serialize)]
enum PromptType {
    FIX,
    GENERATE,
    EMACS,
}

#[derive(Debug, Serialize)]
struct Request {
    prompt_type: PromptType,
    data: String,
}

#[derive(Debug, Deserialize)]
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
    pub fn new(_addr: &str, model: Model) -> Self {
        let n = _addr.parse::<SocketAddr>().unwrap();
        Self { addr: n, model }
    }

    pub async fn start(&self) {
        let app = Router::new().route("/", post(sse_prompt)).with_state(self.model.clone());

        let server = axum::Server::bind(&self.addr)
            .serve(app.into_make_service())
            .with_graceful_shutdown(shutdown_signal());

        println!("Server running on http://{}", &self.addr);

        if let Err(err) = server.await {
            eprintln!("Server error: {}", err);
        }
    }
}
