module Arkham.Homebrew.AgesUnwound.Treacheries.YouMustNotBeSeen (youMustNotBeSeen) where

import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.AWorldTornDownAgain.Helpers (getActDecksInPlay)
import Arkham.Treachery.Import.Lifted

newtype YouMustNotBeSeen = YouMustNotBeSeen TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

youMustNotBeSeen :: TreacheryCard YouMustNotBeSeen
youMustNotBeSeen = treachery YouMustNotBeSeen Cards.youMustNotBeSeen

{- | "Peril. /
Revelation - If only one act deck is in play, You Must Not Be Seen gains surge.
Otherwise, test [agility] (3). For each point you fail by, take 1 horror. If you
fail by 3 or more, you suffer 1 mental trauma and remove You Must Not Be Seen
from the game."
-}
instance RunMessage YouMustNotBeSeen where
  runMessage msg t@(YouMustNotBeSeen attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      decks <- getActDecksInPlay
      if decks <= 1
        then gainSurge attrs
        else do
          sid <- getRandom
          revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      assignHorror iid attrs n
      when (n >= 3) do
        sufferMentalTrauma iid 1
        removeFromGame attrs
      pure t
    _ -> YouMustNotBeSeen <$> liftRunMessage msg attrs
