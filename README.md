# Llm-Copilot
A local ai pair programmer


## Introduction
A local server that can serve post requests of different llm models as a server side events.


## Installation
To install paste the following command into your terminal

``` shell
cargo install --git https://www.github.com/hhamud/llm-copilot 
```

## Running
To run the server while in the folder
``` shell
cargo run --release
```

Send post requests to the server
``` shell
curl -X POST -H "Content-Type: application/json" -d '{ "data": "write a python function that prints hello world"}' http://localhost:3000
```


