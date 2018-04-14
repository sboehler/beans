# Haricot

Haricot is a [plain text accounting](http://plaintextaccounting.org/) tool in the tradition of ]ledger](https://ledger-cli.org), [beancount](https://furius.ca/beancount) and others. Distinctive features and objectives:

* Clean implementation in Haskell
* Aim for the best abstractions
* Native support multi-asset and multi-currency 
* Hopelessly incomplete compared to others :-)

Accounting with Haricot is based on text files, and adopts a subset of the beancount syntax, which I found the easiest to parse.

## Why another ledger clone?

I love Haskell and Finance and want to write my own accounting tool (classical not-invented-her :-). Secondly, other implementations are quite complex and have many features I don't need, while at the same time multi-currency support and mark-to-market valuation and reconciliation, which are key requirements for me, are second class citizens. 

## Project ideas

* Integrate QuickCheck for testing
* Write a ncurses-like tool for analysis
* Good pretty-printing 
