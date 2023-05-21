use llm::{
    load, load_progress_callback_stdout, InferenceFeedback, InferenceRequest, InferenceResponse,
};
use rand;
use std::path::PathBuf;
use std::sync::Arc;

pub fn load_model<M: llm::KnownModel + 'static>(path: &PathBuf) -> Model {
    let model = Model::new::<M>(path);
    model
}

#[derive(Clone)]
pub struct Model {
    pub data: Arc<dyn llm::Model>,
}

impl Model {
    pub fn new<T>(path: &PathBuf) -> Self
    where
        T: llm::KnownModel + 'static,
    {
        // load a GGML model from disk
        let llama = load::<T>(
            // path to GGML file
            path.as_path(),
            // llm::ModelParameters
            Default::default(),
            // overrides
            None,
            // load progress callback
            load_progress_callback_stdout,
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

        let res = session.infer::<std::convert::Infallible>(
            // model to use for text generation
            self.data.as_ref(),
            // randomness provider
            &mut rand::thread_rng(),
            // the prompt to use for text generation, as well as other
            // inference parameters
            &InferenceRequest {
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
