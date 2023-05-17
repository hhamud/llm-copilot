use llm;
use llm::{InferenceFeedback, InferenceResponse};

use rand;
use std::convert::Infallible;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::mpsc;

use axum::{
    extract::State,
    response::sse::{Event, KeepAlive, Sse},
    routing::{get, post},
    Json, Router,
};

use futures_util::stream::{self, Stream};
use tokio_stream::wrappers::ReceiverStream;

#[derive(Clone)]
pub struct Model {
    pub data: Arc<dyn llm::Model>,
}

impl Model {
    pub fn new(pathing: &str) -> Self {
        let mut path = PathBuf::new();
        path.push(std::env::var("HOME").unwrap_or_else(|_| ".".to_owned()));
        path.push(pathing);

        // load a GGML model from disk
        let llama = llm::load::<llm::models::Llama>(
            // path to GGML file
            path.as_path(),
            // llm::ModelParameters
            Default::default(),
            // overrides
            None,
            // load progress callback
            llm::load_progress_callback_stdout,
        )
        .unwrap_or_else(|err| panic!("Failed to load model: {err}"));

        Self {
            data: Arc::new(llama),
        }
    }

    pub fn run_session(&self, prompt: &str) -> impl Stream<Item = Result<Event, Infallible>> {
        // start a new session
        let mut session = self.data.start_session(Default::default());

        // create mpsc channels
        let (tx, mut rx) = mpsc::channel::<Result<Event, Infallible>>(10);

        // use the model to generate text from a prompt
        let res = session.infer::<std::convert::Infallible>(
            // model to use for text generation
            self.data.as_ref(),
            // randomness provider
            &mut rand::thread_rng(),
            // the prompt to use for text generation, as well as other
            // inference parameters
            &llm::InferenceRequest {
                prompt,
                ..Default::default()
            },
            // llm::OutputRequest
            &mut Default::default(),
            // output callback
            |t| match t {
                InferenceResponse::InferredToken(r) => {
                    tx.try_send(Ok(Event::default().data(r)));

                    Ok(InferenceFeedback::Continue)
                } // Handle any errors returned from the inference process
                InferenceResponse::PromptToken(_) |
                InferenceResponse::SnapshotToken(_) |
                InferenceResponse::EotToken
                => {
                    Ok(InferenceFeedback::Continue)
                }
            },
        );

        ReceiverStream::new(rx)
    }
}
