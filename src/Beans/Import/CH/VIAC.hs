module Beans.Import.CH.VIAC (parse) where

import Beans.Command (Command (CmdPrice))
import Beans.Commodity (Commodity (Commodity))
import Beans.Date (Date)
import qualified Beans.Import.Common as Common
import Beans.Price (Price (Price))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Bifunctor (first)
import qualified Data.ByteString as B
import GHC.Generics (Generic)

parse :: Common.Config -> B.ByteString -> Either Common.ImporterException [Command]
parse _ bytes = do
  (s :: A.Value) <- first Common.ImporterException (A.eitherDecodeStrict bytes)
  v <- first Common.ImporterException $ A.parseEither values s
  pure $ CmdPrice . convert <$> v

convert :: TimeSeriesEntry -> Price
convert (TimeSeriesEntry date value) = Price date (Commodity "VIAC") value (Commodity "CHF")

data TimeSeriesEntry
  = TimeSeriesEntry
      { date :: Date,
        value :: Double
      }
  deriving (Generic)

instance A.FromJSON TimeSeriesEntry

values :: A.Value -> A.Parser [TimeSeriesEntry]
values = A.withObject "VIAC data" $ (A..: "dailyValues")