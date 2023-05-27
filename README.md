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

To run the server after installation if the model has already been downloaded:
``` shell
llm-copilot <llama/bloom/gpt2/gptj> -m <model-path> --address <local server address>
```

Or download one from HuggingFace using the username and repository:
``` shell
llm-copilot <llama/bloom/gpt2/gptj> -m <username/repo> --address <local server address>
```

### Emacs:
call function `llm-copilot-start-server` to start the server supplying the model path and address if needed

call function `llm-copilot--generate` to send a prompt to the llm and it generates code and inserts the code response into an org-code block in a seperate temporary org buffer.

### Other IDEs
Send post requests to the server as so if not using emacs
``` shell
curl -X POST -H "Content-Type: application/json" -d '{ "prompt_type": "GENERATE", "data": "write a python function that prints hello world"}' http://localhost:3000
```


## Disclaimer

Please note that this project is currently in a Beta stage. While we have worked to ensure its functionality, it may contain bugs, errors, or other inconsistencies. We appreciate your understanding and invite you to report any issues you encounter. Your feedback is invaluable in improving this project.

By using this project, you acknowledge that you understand the project is in a Beta phase and may not function as expected. Use this at your own discretion. The maintainers of this project are not responsible for any problems that may occur from using this project.
