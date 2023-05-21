use clap::Parser;
mod model;
mod prompts;

use std::path::PathBuf;

use serde::{Deserialize, Serialize};

use crate::model::Model;
use crate::prompts::Prompt;
use axum::{extract::State, routing::post, Json, Router};
use tokio;

#[derive(Debug, Parser)]
enum Args {
    /// Use a LLaMA model
    Llama {
        #[arg(long, short = 'm')]
        model: PathBuf,
    },
    /// Use a BLOOM model
    Bloom {
        #[arg(long, short = 'm')]
        model: PathBuf,
    },
    /// Use a GPT2 model
    #[clap(id = "gpt2")]
    Gpt2 {
        #[arg(long, short = 'm')]
        model: PathBuf,
    },
    /// Use a GPT-J model
    #[clap(id = "gptj")]
    GptJ {
        #[arg(long, short = 'm')]
        model: PathBuf,
    },
}

#[derive(Debug, Deserialize, Serialize)]
struct Request {
    data: String,
}

#[derive(Debug, Deserialize, Serialize)]
struct Tokens {
    data: String,
}

async fn sse_prompt(State(model): State<Model>, message: Json<Request>) -> Json<Tokens> {
    let prompt = Prompt::new("src/prompts/");

    let gen = prompt.generate(&message.data);

    let session = model.run_session(&gen);

    let response = Tokens { data: session };

    Json(response)
}

fn load_model<M: llm::KnownModel + 'static>(path: &PathBuf) -> Model {
    let model = Model::new::<M>(path);
    model
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

#[tokio::main]
async fn main() {
    let cli = Args::parse();

    let model = match &cli {
        Args::Llama { model } => load_model::<llm::models::Llama>(model),
        Args::Bloom { model } => load_model::<llm::models::Bloom>(model),
        Args::Gpt2 { model } => load_model::<llm::models::Gpt2>(model),
        Args::GptJ { model } => load_model::<llm::models::GptJ>(model),
    };

    let app = Router::new().route("/", post(sse_prompt)).with_state(model);

    let addr = "0.0.0.0:3000";

    let server = axum::Server::bind(&addr.parse().unwrap())
        .serve(app.into_make_service())
        .with_graceful_shutdown(shutdown_signal());

    println!("Server running on http://{}", addr);

    if let Err(err) = server.await {
        eprintln!("Server error: {}", err);
    }
}
