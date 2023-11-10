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


```haskell
      let s = weakSession (traceSendAPI "" $ clientSendAPI endpoint)
      h <-
        send s $ do
          h <- reqGetData "acc://9c549cbba290efeb8029184caac3d36c5bfcacb361a29282/ACME"
          return h
      print h
```

## Usage

See API documentation first, to get familliar with API methods https://docs.accumulatenetwork.io/ and https://documenter.getpostman.com/view/1835497/UzBnrSFB

See how it's used in real application here https://github.com/sigrlami/siare/tree/master/siare-accumulate - Siare is an Oracle dApps that brings various types of data on-chain. There you can see examples of getting infromation from chain, creating transactions and writing to data accounts.

## Contributions

The Library developed by Kelecorix, Inc. If you're an active user or find it useful we strongly encourage you to support our efforts and ensure long maintenance by contributing a small donation to one of the following cryptocurrency addresses:

- ACME: acc://dodecahedron.acme/tipjar
- ETH: 0x885cf43dbe00739202d817d87133189dc5d2441d
