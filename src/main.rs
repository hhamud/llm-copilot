mod model;
mod prompts;

use serde::{Deserialize, Serialize};

use crate::model::Model;
use crate::prompts::Prompt;

use axum::{
    extract::State,
    response::sse::{Event, KeepAlive, Sse},
    routing::{get, post},
    Json, Router,
};
use futures_util::stream::{self, Stream};
use std::{convert::Infallible, time::Duration};
use tokio_stream::StreamExt as _;

#[derive(Debug, Deserialize, Serialize)]
struct Request {
    data: String,
}

#[derive(Debug)]
struct SlidingWindow {
    data: String,
}

async fn sse_prompt(
    State(model): State<Model>,
    message: Json<Request>,
) -> Sse<impl Stream<Item = Result<Event, Infallible>>> {
    let prompt = Prompt::new("src/prompts/");
    // context size of model
    let size = model.data.n_context_tokens();

    let gen = prompt.generate(&message.data);

    let session = model.run_session(&gen);

    Sse::new(session).keep_alive(KeepAlive::default())
}

#[tokio::main]
async fn main() {
    let model = Model::new("models/ggml-alpaca-13b-q4.bin");
    let app = Router::new().route("/", post(sse_prompt)).with_state(model);

    axum::Server::bind(&"0.0.0.0:3000".parse().unwrap())
        .serve(app.into_make_service())
        .await
        .unwrap();
}

//curl -X POST -H "Content-Type: application/json" -d '{ "data": "write a python function that prints hello world"}' http://localhost:3000/
