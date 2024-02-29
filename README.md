# iter 🔁

A code iteration tool running on the [Groq](https://groq.com) API.

This is an UX experiment for RHLF-based code iteration. It takes the form
of a REPL with free-form text that lets the user quickly iterate on diffs,
pipe feedback (e.g. compilers and test suites) into the LLM, and trigger
[self-reflection](https://github.com/rxlqn/awesome-llm-self-reflection).

<iframe src="https://www.loom.com/embed/e8c262e754fa4468962a1eec444ab8e3?sid=79729867-a79b-469e-82e4-6e48dfbc8b69" frameborder="0" webkitallowfullscreen mozallowfullscreen allowfullscreen style="position: absolute; top: 0; left: 0; width: 100%; height: 100%;">
</iframe>

## usage

* [Get an API Key](https://wow.groq.com/) from Groq!

* Install [Nix](https://nixos.org/).

* Run `iter` via:

  ```
  export HISTIGNORE=$HISTIGNORE':*GROQ_SECRET_ACCESS_KEY*' # optional, bash only
  export GROQ_SECRET_ACCESS_KEY=<your Groq API key>
  nix run github:freuk/iter#iter
  ```

  Or install the `iter` binary via `nix profile install github:freuk/iter#iter`.

By default, `iter` uses `mixtral-8x7b-32768`, a 32k sequence length MoE
of 7b parameter language models from [Mistral AI](https://mistral.ai/).
Use `--config` (see `demos/` for examples) to change this choice to
`llama2-70b-4096` (the other available model at the time of writing).

## development

`nix-shell` will give you a development environment for `iter`.

* `ghcid`: live GHC feedback
* `just`: useful commands for interactive development
