use git2::Repository;
use llm::{
    load, load_progress_callback_stdout, InferenceFeedback, InferenceRequest, InferenceResponse,
};
use rand;
use std::path::PathBuf;
use std::sync::Arc;

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

    pub fn download(&self, hf: &str) -> Result<(), git2::Error> {
        let hf_repo = HuggingFaceRepo::from_str(hf)
            .map_err(|_| git2::Error::from_str("Invalid Hugging Face repository format"))?;

        let url = format!("https://huggingface.co/{}", hf_repo.to_string());
        let local_path = Path::new(".models").join(&hf_repo.repo);

        // Create directory if it doesn't exist
        if !Path::new(".models").exists() {
            fs::create_dir(".models")?;
        }

        let _repo = match Repository::clone(&url, &local_path) {
            Ok(repo) => repo,
            Err(e) => panic!("Failed to clone: {}", e),
        };

        Ok(())
    }

    pub fn load_from_repo<T>(&self, hf: &str) -> Result<(), Error>
    where
        T: llm::KnownModel + 'static,
    {
        let hf_repo = HuggingFaceRepo::from_str(hf)
            .map_err(|err| Error::new(std::io::ErrorKind::InvalidData, err))?;

        let path = PathBuf::from(".models").join(&hf_repo.repo);

        if let Ok(entries) = fs::read_dir(path) {
            for entry in entries {
                if let Ok(entry) = entry {
                    // If the entry is a file and it has extension .bin
                    if entry.path().is_file()
                        && entry.path().extension().unwrap_or_default() == "bin"
                    {
                        let llama = self.new::<T>(&entry.path());
                        // Now you can use the llama model
                        //...
                        return Ok(());
                    }
                }
            }
        }

        Err(Error::new(
            std::io::ErrorKind::NotFound,
            "Model file not found",
        ))
    }
}

pub struct HuggingFaceRepo {
    username: String,
    repo: String,
}

impl HuggingFaceRepo {
    pub fn new<S: Into<String>>(username: S, repo: S) -> Self {
        Self {
            username: username.into(),
            repo: repo.into(),
        }
    }

    pub fn from_str<S: AsRef<str>>(s: S) -> Result<Self, &'static str> {
        let parts: Vec<&str> = s.as_ref().split('/').collect();
        if parts.len() != 2 {
            Err("Invalid format. Expected 'username/repo'.")
        } else {
            Ok(Self::new(parts[0], parts[1]))
        }
    }

    pub fn to_string(&self) -> String {
        format!("{}/{}", self.username, self.repo)
    }
}
