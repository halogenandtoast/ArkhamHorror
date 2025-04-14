module Arkham.Location.Cards.ToweringLuminosity (toweringLuminosity) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Matcher
import Arkham.Ability

newtype ToweringLuminosity = ToweringLuminosity LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

toweringLuminosity :: LocationCard ToweringLuminosity
toweringLuminosity = location ToweringLuminosity Cards.toweringLuminosity 3 (Static 4)

instance HasAbilities ToweringLuminosity where
  getAbilities (ToweringLuminosity a) =
    extendRevealed1 a $ restricted a 1 Here $ forced $ SkillTestResult #after You AnySkillTest #failure

instance RunMessage ToweringLuminosity where
  runMessage msg l@(ToweringLuminosity attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      chooseOneM iid do
        labeled "Place 1 doom on Towering Luminosity" $ placeDoom (attrs.ability 1) attrs 1
        labeled "Take 2 Damage" $ assignDamage iid (attrs.ability 1) 2
      pure l
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOneM iid do
        when (attrs.clues > 0) do
          labeled "Flip 1 clue on Towering Luminosity to its doom side" do
            flipCluesToDoom attrs 1
        labeled "Discard  Towering Luminosity" $ toDiscardBy iid (attrs.ability 1) attrs
      pure l
    _ -> ToweringLuminosity <$> liftRunMessage msg attrs
