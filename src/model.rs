use llm::{
    load, load_progress_callback_stdout, InferenceFeedback, InferenceRequest, InferenceResponse,
};
use rand;
use std::fs::{create_dir, read_dir};
use std::io::Error;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use crate::repository::Repository;
use crate::utils;

#[derive(Clone)]
pub struct Model {
    pub data: Arc<dyn llm::Model>,
}

impl Model {
    pub fn new<T>(input: &str) -> Result<Self, std::io::Error>
    where
        T: llm::KnownModel + 'static,
    {
        let parts: Vec<&str> = input.split('/').collect();

        if parts.len() == 2 {
            let hf_repo = Repository::from_str(input)
                .map_err(|err| Error::new(std::io::ErrorKind::InvalidData, err))?;

            utils::download(input).unwrap();

            let path = PathBuf::from(".hfmodels").join(&hf_repo.repo);

            if let Ok(entries) = read_dir(&path) {
                for entry in entries {
                    if let Ok(entry) = entry {
                        // If the entry is a file and it has extension .bin
                        if entry.path().is_file()
                            && entry.path().extension().unwrap_or_default() == "bin"
                        {
                            let model = load::<T>(
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

                            return Ok(Self {
                                data: Arc::new(model),
                            });
                        }
                    }
                }
            }

            return Err(Error::new(
                std::io::ErrorKind::NotFound,
                "Model file not found",
            ));
        } else if Path::new(&input).is_absolute() {
            // load a GGML model from disk
            let model = load::<T>(
                // path to GGML file
                Path::new(&input),
                // llm::ModelParameters
                Default::default(),
                // overrides
                None,
                // load progress callback
                load_progress_callback_stdout,
            )
            .unwrap_or_else(|err| panic!("Failed to load model: {err}"));

            return Ok(Self {
                data: Arc::new(model),
            });
        }

        return Err(Error::new(
            std::io::ErrorKind::NotFound,
            "Model file not found",
        ));
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
