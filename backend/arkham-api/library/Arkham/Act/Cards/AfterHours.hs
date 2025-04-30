module Arkham.Act.Cards.AfterHours (afterHours) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Deck qualified as Deck

newtype AfterHours = AfterHours ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

afterHours :: ActCard AfterHours
afterHours = act (1, A) AfterHours Cards.afterHours (groupClueCost (PerPlayer 3))

instance RunMessage AfterHours where
  runMessage msg a@(AfterHours attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      jazzMulligan <- fetchCard Assets.jazzMulligan
      shuffleCardsIntoDeck Deck.EncounterDeck (only jazzMulligan)
      shuffleEncounterDiscardBackIn
      advanceActDeck attrs
      pure a
    _ -> AfterHours <$> liftRunMessage msg attrs
