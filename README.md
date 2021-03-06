# Haskell client for Accumulate

![build](https://github.com/kelecorix/accumulate-haskell-client/workflows/build/badge.svg)
![Hackage](https://img.shields.io/hackage/v/api-rpc-accumulate)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/kelecorix/api-rpc-accumulate/blob/master/LICENSE)

A JSON-RPC Haskell client for the Factom protocol. Each response has special ADT(algebraic data type) that automatically converted from JSON response. Using [Remote Monad](https://ku-fpg.github.io/files/Gill-15-RemoteMonad.pdf) pattern multiple request can be batched and executed simulatenously, following more robust approach and reducing usage of expensive RPC calls.

Choosing a batching strategy. There are two strategies:
- `Weak`   - a single command or a single procedure, or
- `Strong` - a sequence of commands, optionally terminated by a procedure

# Installation

You can install package from [Hackage](https://hackage.haskell.org/package/api-rpc-accumulate) and build with Cabal, but we recommend to use [Stack](https://haskellstack.org) tool. Add to you dependencies in stack.yaml and cabal file dependency `- api-rpc-accumulate`.

To run and test from repository

1. Build with stack
```bash
$ stack build
```
2. Load REPL with stack for evaluation
```
$ stack repl
```

3. execute required methods

## Usage

## Contributions

The Library developed by Kelecorix, Inc. If you're an active user or find it useful we strongly encourage you to support our efforts and ensure long maintenance by contributing a small donation to one of the following cryptocurrency addresses:

- BTC: 39oVXpsgsyW8ZgzsnX3sV7HLdtXWfT96qN
- ETH: 0x9cDBA6bb44772259B3A3fb89cf233A147a720f34
- FCT: FA38cwer93mmPw1HxjScLmK1yF9iJTu5P87T2vdkbuLovm2YXyss
