use llm;
use rand;
use std::path::PathBuf;

use crate::prompts::Prompt;

pub struct Model {
    pub data: Box<dyn llm::Model>,
}

impl Model {
    pub fn new(pathing: &str) -> Self {
        let mut path = PathBuf::new();
        path.push(std::env::var("HOME").unwrap_or_else(|_| ".".to_owned()));
        path.push(pathing);

        //models/ggml-alpaca-13b-q4.bin
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
            data: Box::new(llama),
        }
    }

    pub fn run_session(&self, prompt: &str) -> String {
        let mut res = String::new();
        // use the model to generate text from a prompt
        let mut session = self.data.start_session(Default::default());
        session.infer::<std::convert::Infallible>(
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
            |t| {
                res.push_str(t);
                Ok(())
            },
        );
        res
    }
}
