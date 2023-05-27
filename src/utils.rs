use crate::repository::Repository;
use git2;
use std::fs::{create_dir_all, read_dir};
use std::path::Path;

pub fn download(hf: &str) -> Result<(), git2::Error> {
    let hf_repo = Repository::from_str(hf)
        .map_err(|_| git2::Error::from_str("Invalid Hugging Face repository format"))?;

    let url = format!("https://huggingface.co/{}", hf_repo.to_string());

    let local_path = Path::new(".hfmodels").join(&hf_repo.repo);

    // Create directory if it doesn't exist
    if !local_path.exists() {
        create_dir_all(&local_path).unwrap();
    }

    // Proceed with clone only if directory is empty
    if local_path.read_dir().unwrap().next().is_none() {
        match git2::Repository::clone(&url, &local_path) {
            Ok(repo) => repo,
            Err(e) => panic!("Failed to clone: {}", e),
        };
    }

    Ok(())
}
