use axum::response::sse::Event;
use futures_util::stream::Stream;
use llm;
use llm::{InferenceFeedback, InferenceResponse};
use rand;
use std::convert::Infallible;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::mpsc;
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

    pub fn run_session(&self, prompt: &str) -> String {
        let mut data = String::new();
        // start a new session
        let mut session = self.data.start_session(Default::default());

        // create mpsc channels
        let (tx, rx) = mpsc::channel::<Result<Event, Infallible>>(100);

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
                    data.push_str(&r);

                    Ok(InferenceFeedback::Continue)
                }
                InferenceResponse::PromptToken(_)
                | InferenceResponse::SnapshotToken(_)
                | InferenceResponse::EotToken => Ok(InferenceFeedback::Continue),
            },
        );

        match res {
            Ok(result) => println!("{:?}", result),
            Err(result) => println!("{:?}", result),
        }

        data
    }
}
