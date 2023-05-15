use axum::{
    routing::{get, post},
    Json, Router,
};
mod model;
mod prompts;
use serde::{Deserialize, Serialize};

use crate::model::Model;
use crate::prompts::Prompt;

#[derive(Debug, Deserialize, Serialize)]
struct Response {
    data: String,
}

async fn post_prompt() -> Json<Response> {
    let prompt = Prompt::new("src/prompts/");
    let model = Model::new("models/ggml-alpaca-13b-q4.bin");

    let session = model.run_session(&prompt);

    let res = Response { data: session };

    Json(res)
}

#[tokio::main]
async fn main() {
    // build our application with a single route
    let app = Router::new().route("/", get(post_prompt));

    // run it with hyper on localhost:3000
    axum::Server::bind(&"0.0.0.0:3000".parse().unwrap())
        .serve(app.into_make_service())
        .await
        .unwrap();
}
