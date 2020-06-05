module Beans.Infer
  ( Model,
    train,
    fixDirective,
    updateDirectives,
  )
where

import Beans.Account (Account)
import qualified Beans.Account as Account
import Beans.Command (Command (..), Directive (..))
import Beans.Transaction (Posting (..), Transaction (..))
import Data.Bifunctor (second)
import Data.Function (on)
import Data.HashMap.Strict ((!), HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty

data Model
  = Model
      { def :: Double,
        priors :: HashMap Account Double,
        posteriors :: HashMap (Account, Token) Double
      }

type Token = Text

train :: [Transaction] -> Model
train trx = Model {priors, posteriors, def}
  where
    sets :: [(Account, [Text])]
    sets = [(a, tokenize t p) | t@(Transaction _ _ _ postings) <- trx, p@(Posting a _ _ _ _) <- postings]
    def :: Double
    def = 1.0 / sum accountCount
    accountCount = HashMap.fromListWith (+) . fmap (second (const 1.0)) $ sets
    posteriors = HashMap.fromListWith (+) $ do
      (account, tokens) <- sets
      token <- tokens
      pure ((account, token), 1.0 / accountCount ! account)
    priors = (* def) <$> accountCount

infer :: Model -> [Text] -> Account
infer Model {def, priors, posteriors} tokens = fst $ List.maximumBy (compare `on` snd) likelihoods
  where
    likelihoods = do
      account <- HashMap.keys priors
      let prior = priors ! account
          posterior = do
            token <- tokens
            pure $ HashMap.lookupDefault def (account, token) posteriors
      pure (account, sum (log <$> prior : posterior))

tokenize :: Transaction -> Posting -> [Text]
tokenize (Transaction _ desc _ ps) p = tokens
  where
    tokens = List.nub $ Text.toLower <$> (descTokens ++ accountNames)
    descTokens = Text.words desc
    accountNames = concat $ do
      p'@(Posting a' _ _ _ t') <- ps
      pure $
        if p == p'
          then maybe [] (pure . render) t'
          else [render a' | a' /= Account.unknown]

fixDirective :: Model -> Directive -> Directive
fixDirective m (CmdDirective pos (CmdTransaction t@(Transaction d desc ts ps))) = CmdDirective pos (CmdTransaction (Transaction d desc ts ps'))
  where
    ps' = fixPosting m t <$> ps
fixDirective _ d = d

fixPosting :: Model -> Transaction -> Posting -> Posting
fixPosting m t p@(Posting a c l amt tag)
  | a == Account.unknown = Posting (infer m tokens) c l amt tag
  | otherwise = p
  where
    tokens = tokenize t p

updateDirectives :: [Directive] -> Text -> Text
updateDirectives d text = splUpdate text 0 edits
  where
    edits = [((start, end), render c) | (CmdDirective (start, end) c) <- d]

splUpdate :: Text -> Int -> [((Int, Int), Text)] -> Text
splUpdate text offset (((start, end), newText) : more) = prefix <> newText <> splUpdate suffix (offset + Text.length prefix + Text.length oldText) more
  where
    (prefix, r) = Text.splitAt (start - offset) text
    (oldText, suffix) = Text.splitAt (end - start) r
splUpdate text _ [] = text

render :: Pretty.Pretty a => a -> Text
render = Pretty.renderStrict . Pretty.layoutCompact . Pretty.pretty
