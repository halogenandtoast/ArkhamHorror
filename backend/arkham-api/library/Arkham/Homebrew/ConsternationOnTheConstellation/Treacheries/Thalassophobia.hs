module Arkham.Homebrew.ConsternationOnTheConstellation.Treacheries.Thalassophobia (thalassophobia) where

import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Treacheries qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Thalassophobia = Thalassophobia TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Test [willpower] (2), at +2 difficulty if your location is exhausted, and take 1
horror for each point you fail by.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
thalassophobia :: TreacheryCard Thalassophobia
thalassophobia = treachery Thalassophobia Cards.thalassophobia

instance RunMessage Thalassophobia where
  runMessage msg (Thalassophobia attrs) = runQueueT $ Thalassophobia <$> liftRunMessage msg attrs
