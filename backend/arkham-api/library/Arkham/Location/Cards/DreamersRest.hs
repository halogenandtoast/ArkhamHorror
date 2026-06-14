module Arkham.Location.Cards.DreamersRest (dreamersRest) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype DreamersRest = DreamersRest LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dreamersRest :: LocationCard DreamersRest
dreamersRest = location DreamersRest Cards.dreamersRest 1 (Static 5)

instance HasAbilities DreamersRest where
  getAbilities (DreamersRest a) =
    extendRevealed1 a
      $ mkAbility a 1
      $ forced
      $ SkillTestResult #after (InvestigatorAt $ be a) AnySkillTest #failure

instance RunMessage DreamersRest where
  runMessage msg l@(DreamersRest attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      scenarioSpecific "increaseDisturbance" ()
      pure l
    _ -> DreamersRest <$> liftRunMessage msg attrs
