pub struct Repository {
    pub user: String,
    pub repo: String,
}

impl Repository {
    pub fn new<S: Into<String>>(user: S, repo: S) -> Self {
        Self {
            user: user.into(),
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
        format!("{}/{}", self.user, self.repo)
    }
}
