use clap::Parser;
mod cli;
mod model;
mod prompts;
mod server;

use crate::cli::Args;
use crate::model::{load_model, Model};
use crate::server::Server;


#[tokio::main]
async fn main() {
    let cli = Args::parse();

    let model = match &cli {
        Args::Llama { model } => load_model::<llm::models::Llama>(model),
        Args::Bloom { model } => load_model::<llm::models::Bloom>(model),
        Args::Gpt2 { model } => load_model::<llm::models::Gpt2>(model),
        Args::GptJ { model } => load_model::<llm::models::GptJ>(model),
    };

    let addr = "0.0.0.0:3000";
    let server = Server::new(addr, model);
    server.start().await;
}
