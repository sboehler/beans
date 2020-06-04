[![Build Status](https://travis-ci.org/sboehler/beans.svg?branch=master)](https://travis-ci.org/sboehler/beans)


# beans - A Plain-Text Accounting Tool

`beans` is a [plain text accounting tool](https://plaintextaccounting) in the tradition of [ledger](https://www.ledger-cli.org),
[hledger](https://hledger.org) and [beancount](https://furius.ca/beancount/). Read the overview below, or check out the
[manual](https://sboehler.github.com/beans) for details!


## Track your net worth

Print a formatted balance sheet in USD:

    beans balance -j examples/example.bean

Output:

    |------------------+------------+------------|
    | Account          | 2018-01-01 | 2018-11-01 |
    |------------------+------------+------------|
    | Assets           |            |            |
    |   Checking       |            |            |
    |     USD          |   15000.00 |   14600.00 |
    |   Portfolio      |            |            |
    |     AAPL         |            |      10.00 |
    |   Wallet         |            |            |
    |     BTC          |            |       0.25 |
    |------------------+------------+------------|
    | Equity           |            |            |
    |   Gains          |            |            |
    |     AAPL         |            |     -10.00 |
    |     BTC          |            |      -0.25 |
    |     USD          |            |    5200.00 |
    |   OpeningBalance |            |            |
    |     USD          |  -15000.00 |  -15000.00 |
    |------------------+------------+------------|
    | Income           |            |            |
    |   Salary         |            |            |
    |     USD          |            |  -12000.00 |
    |------------------+------------+------------|
    | Expenses         |            |            |
    |   Books          |            |            |
    |     USD          |            |     100.00 |
    |   Groceries      |            |            |
    |     USD          |            |    1100.00 |
    |   Rent           |            |            |
    |     USD          |            |    6000.00 |
    |------------------+------------+------------|
    | Total            |            |            |
    |   AAPL           |            |            |
    |   BTC            |            |            |
    |   USD            |            |            |
    |------------------+------------+------------|


## Convert arbitrary commodities and currencies

Convert all amounts to USD, using latest market prices:

    beans balance -j examples/example.bean --val USD

Output:

    |------------------+------------+------------|
    | Account          | 2018-01-01 | 2018-11-01 |
    |------------------+------------+------------|
    | Assets           |            |            |
    |   Checking       |   15000.00 |   14600.00 |
    |   Portfolio      |            |    2220.00 |
    |   Wallet         |            |    1589.50 |
    |------------------+------------+------------|
    | Equity           |            |            |
    |   Equity         |            |    1233.50 |
    |   Gains          |            |     157.00 |
    |   OpeningBalance |  -15000.00 |  -15000.00 |
    |------------------+------------+------------|
    | Income           |            |            |
    |   Salary         |            |  -12000.00 |
    |------------------+------------+------------|
    | Expenses         |            |            |
    |   Books          |            |     100.00 |
    |   Groceries      |            |    1100.00 |
    |   Rent           |            |    6000.00 |
    |------------------+------------+------------|
    | Total            |            |            |
    |------------------+------------+------------|

Convert all amounts to bitcoin, using latest market prices:

    beans balance -j examples/example.bean --val BTC

Output:

    |------------------+------------+------------|
    | Account          | 2018-01-01 | 2018-11-01 |
    |------------------+------------+------------|
    | Assets           |            |            |
    |   Checking       |       1.12 |       2.30 |
    |   Portfolio      |            |       0.35 |
    |   Wallet         |            |       0.25 |
    |------------------+------------+------------|
    | Equity           |            |            |
    |   Equity         |            |      -1.34 |
    |   Gains          |            |       0.01 |
    |   OpeningBalance |      -1.12 |      -1.12 |
    |------------------+------------+------------|
    | Income           |            |            |
    |   Salary         |            |      -1.11 |
    |------------------+------------+------------|
    | Expenses         |            |            |
    |   Books          |            |       0.01 |
    |   Groceries      |            |       0.09 |
    |   Rent           |            |       0.55 |
    |------------------+------------+------------|
    | Total            |       0.00 |       0.00 |
    |------------------+------------+------------|


## Importers and Bayesian Inference

Take a sample bank statement:

    Datum von:;2017-05-23
    Buchungsart:;Alle Buchungen
    Konto:;CHXXXXXXXXXXXXXXXXXXX
    Währung:;CHF
    Buchungsdatum;Avisierungstext;Gutschrift;Lastschrift;Valuta;Saldo
    2017-06-01;"Acme Corp";;-135.00;2017-06-01;

    Disclaimer:
    Dies ist kein durch PostFinance AG erstelltes Dokument. PostFinance AG ist nicht verantwortlich für den Inhalt.

Import the statement and assign accounts to bookings:

    beans import -i ch.postfinance -a Assets:Checking examples/quick/postfinance.csv

Output:

    2017-06-01 "Acme Corp"
    Assets:Checking -135 CHF
    TBD 135 CHF

Use Bayesian inference (aka "machine learning") to automatically
assign accounts to the TBD account. Use your journal both as training
and target file, and `beans` will replace TBD accounts with its best
guess, based on your existing data.

    beans infer --help

    Usage: beans infer (-t|--training-file TRAINING_FILE) TARGET_FILE
      Infer accounts

    Available options:
      -t,--training-file TRAINING_FILE
                               The file containing the training data
      -h,--help                Show this help text

    2017-06-01 "Acme Corp"
    Assets:Checking -135 CHF
    TBD 135 CHF


## Import data from Yahoo! and AlphaVantage

`beans` has built-in fetchers for prices from Yahoo! and
AlphaVantage. Provide a configuration in `dhall` (see example) and
`beans` will keep up-to-date price files on request.

    beans fetch --help

    Usage: beans fetch [-c|--commodities COMMODITY] CONFIG_FILE
      Fetch latest prices

    Available options:
      -c,--commodities COMMODITY
                               The commodity to fetch
      CONFIG_FILE              The dhall config file to parse
      -h,--help                Show this help text


## Other features

-   flexible analysis and filtering
-   written in Haskell

Check out the [manual ](https://sboehler.github.io/beans)!


## License

BSD-3
