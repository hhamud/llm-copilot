use std::collections::HashMap;
use std::fs::{read_dir, read_to_string};
use std::path::Path;

pub struct Prompt {
    pub data: HashMap<String, String>,
}

impl Prompt {
    pub fn new(path: &str) -> Self {
        let dir_path = Path::new(path);

        let mut data = HashMap::new();

        let dir_entries = read_dir(dir_path).expect("failed to read dir");

        for entry in dir_entries {
            let entry = entry.unwrap();
            let file_path = entry.path();

            if file_path.is_file() {
                if let Some(file_stem) = file_path.file_stem() {
                    let file_name: String = file_stem.to_string_lossy().to_string();

                    let file_contents: String =
                        read_to_string(&file_path).expect("failed to read contents");

                    data.insert(file_name, file_contents);
                }
            }
        }

        Self { data }
    }

    pub fn fix(&self, fix: &str) -> String {
        let contents = self.data.get("fix").expect("not available");
        contents.replace("{fix}", fix)
    }

    pub fn generate(&self, generate: &str) -> String {
        let contents = self.data.get("generation").expect("not available");
        contents.replace("{generate}", generate)
    }

    pub fn emacs(&self, generate: &str) -> String {
        let contents = self.data.get("emacs").expect("not available");
        contents.replace("{emacs}", generate)
    }
}
