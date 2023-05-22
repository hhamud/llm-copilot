# Llm-Copilot
A local ai pair programmer using local models


## Introduction
A local server that can serve post requests of different llm models.


## Installation
To install paste the following command into your terminal

``` shell
cargo install --git https://www.github.com/hhamud/llm-copilot 
```

## Running
To run the server while in the folder
``` shell
cargo run --release -- llama -m <model-path>
```

### Emacs:
call function `llm-copilot--generate` to send a prompt to the llm and it generates code and inserts the code response into an org-code block in a seperate temporary org buffer.

### Other IDEs
Send post requests to the server as so if not using emacs
``` shell
curl -X POST -H "Content-Type: application/json" -d '{ "prompt_type": "GENERATE", "data": "write a python function that prints hello world"}' http://localhost:3000
```
