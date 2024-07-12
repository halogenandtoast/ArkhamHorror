module Arkham.GHCI where

import Arkham.Card
import Arkham.Card.EncounterCard
import Arkham.Card.PlayerCard
import Arkham.Prelude

newtype GHCI a = GHCI {runGHCI :: IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadRandom)

instance CardGen GHCI where
  genEncounterCard a = do
    cardId <- GHCI $ unsafeMakeCardId <$> getRandom
    pure $ lookupEncounterCard (toCardDef a) cardId
  genPlayerCard a = do
    cardId <- GHCI $ unsafeMakeCardId <$> getRandom
    pure $ lookupPlayerCard (toCardDef a) cardId
  replaceCard _ _ = pure ()
  clearCardCache = pure ()
