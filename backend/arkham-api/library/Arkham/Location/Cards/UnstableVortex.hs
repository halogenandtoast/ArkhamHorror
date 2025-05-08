module Arkham.Location.Cards.UnstableVortex (unstableVortex) where

import Arkham.Ability
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.LostInTimeAndSpace.Helpers

newtype UnstableVortex = UnstableVortex LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unstableVortex :: LocationCard UnstableVortex
unstableVortex = location UnstableVortex Cards.unstableVortex 0 (Static 0)

instance HasAbilities UnstableVortex where
  getAbilities (UnstableVortex a) =
    extendRevealed1 a $ restricted a 1 Here $ forced $ TurnEnds #when You

instance RunMessage UnstableVortex where
  runMessage msg l@(UnstableVortex attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      tearThroughSpace <- select $ locationIs Cards.tearThroughSpace
      unless (null tearThroughSpace) do
        chooseOneM iid do
          scenarioI18n $ questionLabeled' "unstableVortex.chooseTearThroughSpace"
          targets tearThroughSpace $ toDiscardBy iid attrs
      pure l
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOneM iid do
        withI18n $ labeled' "drawTopCardOfEncounterDeck" $ drawEncounterCard iid (attrs.ability 1)
        scenarioI18n $ labeled' "unstableVortex.shuffle" $ shuffleBackIntoEncounterDeck attrs
      pure l
    _ -> UnstableVortex <$> liftRunMessage msg attrs
