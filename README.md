# Brainfuck

## Instructions

This is another Brainfuck interpreter using [Rust](https://www.rust-lang.org) language. Why not ?

This repository only holds the core logic of the interpreter (lexer and parser). It supports two runnable modes when executing the Brainfuck code:

- Strict mode: return an error when the memory address or value is out of bounds.
- Wrap mode: wrap the memory address or value when it is out of bounds.

By default, the pool of memory is initialized with a size of 30 000 cells. It can be customized when instantiating the parser.

No optimizations are applied to the generated code. The interpreter is designed to be as simple and straightforward as possible, with a focus on readability and maintainability. One interesting thing to note is that the lexer has a one-to-one mapping between the Brainfuck instructions and the token with the exception of the start and end of loop instructions. The generated tokens will contain the boundary position of the loop:

- If it's start token, it'll contain the index of the corresponding end token
- And, if it's end token, it'll contain the index of the corresponding start token

## CI / CD

The CI/CD pipeline is configured using GitHub Actions. The workflow is defined in the [`.github/workflows`](.github/workflows) folder:

- Static Analysis (source code, GitHub Actions)
- Tests (unit tests with code coverage generated)
- Code Audit (on each Cargo dependencies update, or run each day through CronJob)

Additionally, Dependabot is configured to automatically update dependencies (GitHub Actions, Cargo dependencies).

## Repository configuration

The settings of this repository are managed from the [gitops-deployments](https://github.com/jaudiger/gitops-deployments) repository using Terraform. The actual configuration applied is located in the Terraform module [`modules/github-repository`](https://github.com/jaudiger/gitops-deployments/tree/main/modules/github-repository).
