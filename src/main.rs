use axum::{routing::get, Router};

mod prompts;
mod model;


use crate::prompts::Prompt;
use crate::model::Model;

async fn post_prompt() {
    //takes prompt from terminal or emacs
    //sends prompt to server
}

async fn get_prompt_response() {
    // sends back generated prompt from server
}

#[tokio::main]
async fn main() {
    // build our application with a single route
    let app = Router::new().route("/", get(|| async { "Hello, World!" }));

    // run it with hyper on localhost:3000
    axum::Server::bind(&"0.0.0.0:3000".parse().unwrap())
        .serve(app.into_make_service())
        .await
        .unwrap();

    let prompt = Prompt::new("src/prompts/");
    let model = Model::new("models/ggml-alpaca-13b-q4.bin");

    model.run_session()
}
