# iter 🔁

A code iteration tool running on the [Groq](https://groq.com) API.

This is an UX experiment for RHLF-based code iteration. It takes the form
of a REPL with free-form text that lets the user quickly iterate on diffs,
pipe feedback (e.g. compilers and test suites) into the LLM, and trigger
[self-reflection](https://github.com/rxlqn/awesome-llm-self-reflection).

[![asciicast](https://asciinema.org/a/yylhwNUs5WAfSgicOpV3dOkhg.svg)](https://asciinema.org/a/yylhwNUs5WAfSgicOpV3dOkhg)

<details open>
<summary>Hello</summary>

```
# todo: remove this code block before sending to github
GROQ_SECRET_ACCESS_KEY=...
cd demo
asciinema rec
ls
cat demo.yml
iter --cfg=demo.yml demo.py
:d what does this do?
remove the currency data, and functionality
a
remove the '--file' argument and functionality
a
also remove the function to load customers a from a file
a
add a '--test' option that triggers unit tests
a
:f
:f pylint
a
improve various software engineering aspects, but don't make the file longer.
r
:f execute
:f execute
```

</details>


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
