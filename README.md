# iter 🔁

A code iteration tool running on the [Groq API](https://console.groq.com). 

This is an UX experiment and demo for code iteration with
[RHLF-based](https://en.wikipedia.org/wiki/Reinforcement_learning_from_human_feedback)
LLMs. It takes the form of a
[REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop)
with free-form text that lets the user quickly iterate on diffs and pipe
feedback (e.g. compilers and test suites) into the LLM before triggering
[self-reflection](https://github.com/rxlqn/awesome-llm-self-reflection).

[![Video demo](https://img.youtube.com/vi/eR855VNPjhk/0.jpg)](https://www.youtube.com/watch?v=eR855VNPjhk)

## usage

* [Create an account and generate your API key](https://console.groq.com) from Groq!

* Install [Nix](https://nixos.org/).

* Install `iter` via:

  ```
  nix profile install github:freuk/iter#iter
  ```

  Or directly run the `iter` binary via `nix run github:freuk/iter#iter`.

By default, `iter` uses `mixtral-8x7b-32768`, a 32k sequence length MoE
of 7b parameter language models from [Mistral AI](https://mistral.ai/).
Use `--config` (see `demos/` for examples) to change this choice to one of the other available models.

## development

`nix-shell` will give you a development environment for `iter`.

* `ghcid`: live GHC feedback
* `just`: useful commands for interactive development
