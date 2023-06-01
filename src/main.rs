use clap::Parser;
use env_logger;
use log;
mod cli;
mod model;
mod prompts;
mod repository;
mod server;
mod utils;

use crate::cli::Args;
use crate::server::load_server;

#[tokio::main]
async fn main() {
    env_logger::builder()
        .filter_level(log::LevelFilter::Info)
        .parse_default_env()
        .init();

    let cli = Args::parse();

    match &cli {
        Args::Llama { args } => load_server::<llm::models::Llama>(&args.model, &args.address).await,
        Args::Bloom { args } => load_server::<llm::models::Bloom>(&args.model, &args.address).await,
        Args::Gpt2 { args } => load_server::<llm::models::Gpt2>(&args.model, &args.address).await,
        Args::GptJ { args } => load_server::<llm::models::GptJ>(&args.model, &args.address).await,
    }
}
