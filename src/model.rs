use llm;
use llm::{InferenceError, InferenceResponse};
use rand;
use std::path::PathBuf;
use std::sync::Arc;

use axum::{
    extract::State,
    response::sse::{Event, KeepAlive, Sse},
    routing::{get, post},
    Json, Router,
};

use futures_util::stream::{self, Stream};

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
            // load progress callback
            llm::load_progress_callback_stdout,
        )
        .unwrap_or_else(|err| panic!("Failed to load model: {err}"));

        Self {
            data: Arc::new(llama),
        }
    }

    pub fn run_session(&self, prompt: &str) -> impl Stream<Item = Result<Event, Infallible>> {
        let mut res = String::new();
        // start a new session
        let mut session = self.data.start_session(Default::default());

        let (tx, rx) = futures::channel::mpsc::unbounded::<Token>();


    // let stream = stream::collect(move || Event::default().data(&session))
        // .map(Ok)
        // .throttle(Duration::from_secs(1));

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
                    let _ = tx.unbounded_send_all(stream::iter(r || Event::default().data(r)));

                    Ok(InferenceFeedback::Continue)
                } // Handle any errors returned from the inference process
            },
        );

        if let Err(e) = result {
            // Handle the error according to your needs
            // For now, simply print the error message
            println!("Inference error: {:?}", e);
        }
        rx
    }
}
