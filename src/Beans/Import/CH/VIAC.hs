module Beans.Import.CH.VIAC (parse) where

import qualified Beans.Account as Account
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
parse config bytes = do
  v <- first Common.ImporterException $ (A.eitherDecodeStrict bytes >>= A.parseEither values)
  pure $ CmdPrice . convert commodity <$> v
  where
    Common.Config {account} = config
    commodity = Commodity . last $ Account.split account

convert :: Commodity -> TimeSeriesEntry -> Price
convert c (TimeSeriesEntry date value) = Price date c value (Commodity "CHF")

data TimeSeriesEntry
  = TimeSeriesEntry
      { date :: Date,
        value :: Double
      }
  deriving (Generic)

instance A.FromJSON TimeSeriesEntry

values :: A.Value -> A.Parser [TimeSeriesEntry]
values = A.withObject "VIAC data" $ (A..: "dailyValues")
