module Arkham.Location.Cards.ChamberOfNight (chamberOfNight) where

import Arkham.Ability
import Arkham.Card
import Arkham.Helpers.Scenario (scenarioField)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Types (Field (..))

newtype ChamberOfNight = ChamberOfNight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chamberOfNight :: LocationCard ChamberOfNight
chamberOfNight = location ChamberOfNight Cards.chamberOfNight 3 (PerPlayer 2)

instance HasAbilities ChamberOfNight where
  getAbilities (ChamberOfNight a) =
    extendRevealed1 a $ skillTestAbility $ restricted a 1 Here actionAbility

instance RunMessage ChamberOfNight where
  runMessage msg l@(ChamberOfNight attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #intellect (Fixed 4)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      cards <- scenarioField ScenarioCardsUnderScenarioReference
      focusCards cards do
        chooseOrRunOneM iid $ targets cards $ handleTarget iid (attrs.ability 1)
      pure l
    HandleTargetChoice _iid (isAbilitySource attrs 1 -> True) (CardIdTarget cid) -> do
      revealCard . forceFlipCard =<< fetchCard cid
      pure l
    _ -> ChamberOfNight <$> liftRunMessage msg attrs
