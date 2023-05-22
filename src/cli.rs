use clap::Parser;
use std::path::PathBuf;

#[derive(Debug, Clone, Parser)]
pub struct BaseArgs {
    #[arg(long, short = 'm')]
    pub model: PathBuf,

    #[arg(long, short = 'a', default_value = "0.0.0.0:3000")]
    pub address: String,
}

#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
pub enum Args {
    /// Use a LLaMA model
    Llama {
        #[command(flatten)]
        args: BaseArgs,
    },
    /// Use a BLOOM model
    Bloom {
        #[command(flatten)]
        args: BaseArgs,
    },
    /// Use a GPT2 model
    #[clap(id = "gpt2")]
    Gpt2 {
        #[command(flatten)]
        args: BaseArgs,
    },
    /// Use a GPT-J model
    #[clap(id = "gptj")]
    GptJ {
        #[command(flatten)]
        args: BaseArgs,
    },
}
