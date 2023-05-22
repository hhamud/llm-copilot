# Llm-Copilot
A local ai pair programmer using local models


## Installation
To install paste the following command into your terminal

``` shell
cargo install --git https://www.github.com/hhamud/llm-copilot 
```

## Running
Make sure that you have already downloaded a ggml based model that has the following architectures:
- llama
- bloom
- Gpt2
- GptJ


To run the server while in the folder
``` shell
cargo run --release -- <llama/bloom/gpt2/gptj> -m <model-path> --address <local server address>
```


### Emacs:
call function `llm-copilot-start-server` to start the server supplying the model path and address if needed

call function `llm-copilot--generate` to send a prompt to the llm and it generates code and inserts the code response into an org-code block in a seperate temporary org buffer.

### Other IDEs
Send post requests to the server as so if not using emacs
``` shell
curl -X POST -H "Content-Type: application/json" -d '{ "prompt_type": "GENERATE", "data": "write a python function that prints hello world"}' http://localhost:3000
```
