use std::collections::Hashmap;
use std::fs::{read_dir, File};
use std::io::Read;
use std::io::Write;
use std::path::{Path, PathBuf};


struct Prompt {
    data: Hashmap<String, String>,
}

impl Prompt {
    fn new(path: &str) -> Self {
        // load the entire directory
        let dir_path = Path::new(path);

        let data = Hashmap::new();

        // Read directory contents
        let dir_entries = fs::read_dir(dir_path).expect("failed to read dir");

        for entry in dir_entries {
            let file_path = entry.path();

            if file_path.is_file() {
                // Extract the file name without extension
                if let Some(file_stem) = file_path.file_stem() {
                    // Convert the file stem to a string
                    let file_name = file_stem.to_string_lossy();

                    // Read the file contents into a string
                    let file_contents =
                        fs::read_to_string(&file_path).expect("failed to read contents");

                    data.insert(file_name, file_contents);

                    // Process the file contents
                    println!("File: {}", file_path.display());
                    println!("Contents: {}", file_contents);
                }
            }
        }

        Self { data }
    }

    fn fix(&self, fix: &str) -> String {
        let contents = self.data.get("fix").expect("not available");
        contents.replace("{fix}", fix)
    }

    fn generate(&self, generate: &str) -> String {
        self.data.replace("{generate}", generate)
    }
}
