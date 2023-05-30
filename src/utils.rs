use crate::repository::Repository;
use git2;
use std::fs::{create_dir_all, read_dir};
use std::path::Path;
use std::process::Command;
use dirs;

pub fn download(hf: &str) -> Result<(), git2::Error> {
    let hf_repo = Repository::from_str(hf)
        .map_err(|_| git2::Error::from_str("Invalid Hugging Face repository format"))?;

    let url = format!("https://huggingface.co/{}", hf_repo.to_string());

    let path = dirs::home_dir().expect("Failed to get home directory");

    let local_path = path.join(".models").join(&hf_repo.repo);

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

    match git_lfs_pull(&local_path) {
        Ok(()) => println!("Completed successfully."),
        Err(e) => eprintln!("Failed with error: {}", e),
    }

    Ok(())
}

fn git_lfs_pull(path: &Path) -> std::io::Result<()> {
    // user must have git lfs for this hack to work
    let mut output = Command::new("git")
        .current_dir(path)
        .arg("lfs")
        .arg("pull")
        .spawn()?;

    let encode = output.wait()?;

    if encode.success() {
        print!("Success");
    } else {
        print!("Error");
    }

    Ok(())
}
