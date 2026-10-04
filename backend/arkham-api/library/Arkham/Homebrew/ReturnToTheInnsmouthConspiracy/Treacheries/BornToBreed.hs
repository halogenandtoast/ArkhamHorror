module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Treacheries.BornToBreed (bornToBreed) where

import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers (hasMemory)
import Arkham.Campaigns.TheInnsmouthConspiracy.Memory
import Arkham.Card
import Arkham.Deck
import Arkham.Enemy.CardDefs.TheInnsmouthConspiracy.ALightInTheFog qualified as Enemies
import Arkham.Helpers.Scenario (getEncounterDiscard)
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Treacheries qualified as Cards
import Arkham.Scenario.Deck (ScenarioEncounterDeckKey (RegularEncounterDeck))
import Arkham.Treachery.Import.Lifted

newtype BornToBreed = BornToBreed TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bornToBreed :: TreacheryCard BornToBreed
bornToBreed = treachery BornToBreed Cards.bornToBreed

{- | Note the inversion the designer calls out: this surges when it *does* something,
not when it does nothing. Once the lifecycle memory is recovered it is a free draw.
-}
instance RunMessage BornToBreed where
  runMessage msg t@(BornToBreed attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      hasMemory TheLifecycleOfADeepOne >>= \case
        True -> toDiscard attrs attrs
        False -> do
          hatchlings <-
            filter ((== Enemies.deepOneHatchling) . toCardDef)
              . map toCard
              <$> getEncounterDiscard RegularEncounterDeck
          shuffleCardsIntoDeck EncounterDeck hatchlings
          gainSurge attrs
      pure t
    _ -> BornToBreed <$> liftRunMessage msg attrs
