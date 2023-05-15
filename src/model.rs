use llm;
use llm::Model;
use rand;
use std::fs::{read_dir, File};
use std::io::Read;
use std::io::Write;
use std::path::{Path, PathBuf};


struct Model {
    data: llm::model::KnownModel,
}

impl Model {
    fn new(path: &str) -> Self {
        let mut path = PathBuf::new();
        path.push(std::env::var("HOME").unwrap_or_else(|_| ".".to_owned()));
        path.push(path);

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

        Self { data: llama }
    }

    fn run_session(&self, prompt: &Prompt) -> Result<InferenceStats, InferenceError> {
        // use the model to generate text from a prompt
        let mut session = self.data.start_session(Default::default());
        let res = session.infer::<std::convert::Infallible>(
            // model to use for text generation
            &llama,
            // randomness provider
            &mut rand::thread_rng(),
            // the prompt to use for text generation, as well as other
            // inference parameters
            &llm::InferenceRequest {
                prompt: &prompt.generate(
                    "Write the Python code with detailed comments to download webpage content",
                ),
                ..Default::default()
            },
            // llm::OutputRequest
            &mut Default::default(),
            // output callback
            |t| {
                print!("{t}");
                //std::io::stdout().flush().unwrap();

                Ok(())
            },
        );

        match res {

        }
    }
}
