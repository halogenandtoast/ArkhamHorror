module Arkham.Location.Cards.TemporaryHQ (temporaryHQ) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheBlobThatAteEverything.Helpers
import Arkham.Token qualified as Token

newtype TemporaryHQ = TemporaryHQ LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

temporaryHQ :: LocationCard TemporaryHQ
temporaryHQ = locationWith TemporaryHQ Cards.temporaryHQ 2 (Static 0) connectsToAdjacent

instance HasAbilities TemporaryHQ where
  getAbilities (TemporaryHQ a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "temporaryHQ.action"
      $ restricted a 1 Here
      $ actionAbilityWithCost (SpendTokenCost Token.Resource (TargetIs ScenarioTarget))

instance RunMessage TemporaryHQ where
  runMessage msg l@(TemporaryHQ attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOneM iid $ scenarioI18n do
        labeled' "temporaryHQ.healDamage" $ healDamage iid (attrs.ability 1) 3
        labeled' "temporaryHQ.healHorror" $ healHorror iid (attrs.ability 1) 3
        labeled' "temporaryHQ.gainResources" $ gainResources iid (attrs.ability 1) 5
        labeled' "temporaryHQ.drawCards" $ drawCards iid (attrs.ability 1) 3
      pure l
    _ -> TemporaryHQ <$> liftRunMessage msg attrs
