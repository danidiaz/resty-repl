# resty-repl

# What's this?

A program that lets you start some repl and exposes it as REST service.

You can send commands to the REPL, check the history from the point some
command was sent, or check the whole history.

`resty-repl` makes no attempt to detect when some command has finished or
already emitted some output. That it left to the client.

# What are the endpoints?

- GET `/history` 

  Shows the repl's stdout from the very beginning. 

- POST `/interaction` 

  Sends some command to the REPL.

  The only allowed `Content-Type` is `text/plain; charset=utf-8`.

  Returns the link to an "interaction" resource.

- POST `/interaction/:id` 

  Shows the repl's stdout from the point the command :id was sent.

  Notice that this shows also the output of further commands sent to the
  repl!

# Building

Use a modern version (>=2) of [cabal-install](https://www.haskell.org/cabal/).

To build:

    cabal new-build 
    cabal new-install

To run: 

    cabal new-exec -- resty-repl --help
    cabal new-exec -- resty-repl -p 8000 -e "/usr/local/bin/ghci"

# Testing

Once the server is up and running, here are a few curls you can try.

This should show the full repl history:

    curl localhost:8000/history

This sends a command to the repl:

    curl -X POST -H "Content-Type: text/plain; charset=utf-8" --data ":t True"  localhost:8000/interaction

It should respond with a JSON object like:

    {"link":"interaction/0"}

You can then inspect the created resource with

    curl localhost:8000/interaction/0

Remember that you can use `--verbose` with curl for more detailed output.

# Limitations

Many, but among them:

- stderr from the REPL is silently discarded. Might get confusing if you
  send an incorrect command and expect to see something on the history.

- You can't delete resources.

# Motivation

[This seemed like a good
idea](https://www.reddit.com/r/haskell/comments/948s9q/running_ghci_in_background/).
