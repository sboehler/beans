let Config =
      < AVTS : { symbol : Text }
      | AVFX : { fromCurrency : Text, toCurrency : Text }
      | YTS : { symbol : Text }
      >

let Entry
    : Type
    = { commodity : Text, targetCommodity : Text, file : Text, config : Config }

in    [ { commodity = "USD"
        , targetCommodity = "CHF"
        , file = "/home/silvio/test/USD.prices"
        , config = Config.AVFX { fromCurrency = "USD", toCurrency = "CHF" }
        }
      , { commodity = "AAPL"
        , targetCommodity = "USD"
        , file = "prices/AAPL.prices"
        , config = Config.YTS { symbol = "AAPL" }
        }
      ]
    : List Entry
