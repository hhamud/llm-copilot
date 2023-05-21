use clap::Parser;
use std::path::PathBuf;

#[derive(Debug, Parser)]
pub enum Args {
    /// Use a LLaMA model
    Llama {
        #[arg(long, short = 'm')]
        model: PathBuf,
    },
    /// Use a BLOOM model
    Bloom {
        #[arg(long, short = 'm')]
        model: PathBuf,
    },
    /// Use a GPT2 model
    #[clap(id = "gpt2")]
    Gpt2 {
        #[arg(long, short = 'm')]
        model: PathBuf,
    },
    /// Use a GPT-J model
    #[clap(id = "gptj")]
    GptJ {
        #[arg(long, short = 'm')]
        model: PathBuf,
    },
}
