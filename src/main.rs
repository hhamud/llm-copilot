use llm::Model;
use rand;
use std::fs::File;
use std::io::Read;
use std::io::Write;
use std::path::PathBuf;

use jsonrpc_v2::{Data, Error, Params, Server};

#[derive(serde::Deserialize)]
struct TwoNums {
    a: usize,
    b: usize,
}

async fn add(Params(params): Params<TwoNums>) -> Result<usize, Error> {
    Ok(params.a + params.b)
}

async fn sub(Params(params): Params<(usize, usize)>) -> Result<usize, Error> {
    Ok(params.0 - params.1)
}

async fn message(data: Data<String>) -> Result<String, Error> {
    Ok(String::from(&*data))
}



struct Prompt {
    data: String,
}

impl Prompt {
    fn new(path: &str) -> Self {
        let mut file = File::open(path).expect("cannot find file");

        let mut data = String::new();
        file.read_to_string(&mut data).expect("cannot read file");

        Self { data }
    }

    fn fix(&self, fix: &str) -> String {
        self.data.replace("{fix}", fix)
    }

    fn generate(&self, generate: &str) -> String {
        self.data.replace("{generate}", generate)
    }
}


struct Model {
    data: String
}

impl Model {
    fn new() -> Self {

    }
}

fn main() {
    let mut path = PathBuf::new();
    path.push(std::env::var("HOME").unwrap_or_else(|_| ".".to_owned()));
    path.push("models/ggml-alpaca-13b-q4.bin");

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

    let prompt = Prompt::new("src/prompts/generation.txt");

    // use the model to generate text from a prompt
    let mut session = llama.start_session(Default::default());
    let res = session.infer::<std::convert::Infallible>(
        // model to use for text generation
        &llama,
        // randomness provider
        &mut rand::thread_rng(),
        // the prompt to use for text generation, as well as other
        // inference parameters
        &llm::InferenceRequest {
            prompt: &prompt.generate("Write the Python code with detailed comments to download webpage content"),
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

    //match res {
        //Ok(result) => println!("\n\nInference stats:\n{result}"),
        //Err(err) => println!("\n{err}"),
    //}


}


#[actix_rt::main]
async fn main() -> std::io::Result<()> {
    let rpc = Server::new()
        .with_data(Data::new(String::from("Hello!")))
        .with_method("sub", sub)
        .with_method("message", message)
        .finish();

    actix_web::HttpServer::new(move || {
        let rpc = rpc.clone();
        actix_web::App::new().service(
            actix_web::web::service("/api")
                .guard(actix_web::guard::Post())
                .finish(rpc.into_web_service()),
        )
    })
    .bind("0.0.0.0:3000")?
    .run()
    .await
}
