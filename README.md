# Beans

Beans is a [plain text accounting](http://plaintextaccounting.org/) tool in the tradition of [ledger](https://ledger-cli.org), [beancount](https://furius.ca/beancount) and others. Distinctive features and objectives:

* Clean implementation in Haskell
* Aim for the best abstractions
* Natively support multi-asset and multi-currency 
* Hopelessly incomplete compared to others

Accounting with Beans is based on text files, and adopts a subset of the beancount syntax, which I found the easiest to parse.

## Compiling

### Stack

```bash
stack build
```

### Nix / NixOS

```bash
nix-shell
cabal configure
cabal build
```

## Running the example

```bash

# Show the help text
cabal run

# Show the holdings
cabal run beans -- -j examples/example1.bean balance 

# Valuate in your preferred currency
cabal run beans -- -j examples/example1.bean balance --at-value CHF 

```

## Project ideas

* Use QuickCheck for testing
* Create a complete journal for test data
* Write a ncurses-like tool for analysis
* Command line options
* Good pretty-printing 
