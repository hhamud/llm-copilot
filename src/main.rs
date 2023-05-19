mod model;
mod prompts;

use serde::{Deserialize, Serialize};

use crate::model::Model;
use crate::prompts::Prompt;

use axum::{
    extract::State,
    response::sse::{Event, KeepAlive, Sse},
    routing::post,
    Json, Router,
};
use futures_util::stream::Stream;
use std::convert::Infallible;

#[derive(Debug, Deserialize, Serialize)]
struct Request {
    data: String,
}

async fn sse_prompt(
    State(model): State<Model>,
    message: Json<Request>,
) -> Sse<impl Stream<Item = Result<Event, Infallible>>> {
    let prompt = Prompt::new("src/prompts/");

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
