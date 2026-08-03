module Arkham.Location.Cards.DreamersRest (dreamersRest) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.SepulchreOfTheSleeper.Helpers

newtype DreamersRest = DreamersRest LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dreamersRest :: LocationCard DreamersRest
dreamersRest = location DreamersRest Cards.dreamersRest 1 (Static 5)

instance HasAbilities DreamersRest where
  getAbilities (DreamersRest a) =
    -- [Forced] After an investigator fails a test at this location: place 1
    -- resource on the scenario reference card, as Disturbance.
    extendRevealed1 a
      $ mkAbility a 1
      $ forced
      $ SkillTestResult #after (InvestigatorAt $ be a) AnySkillTest #failure

instance RunMessage DreamersRest where
  runMessage msg l@(DreamersRest attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      increaseDisturbance
      pure l
    _ -> DreamersRest <$> liftRunMessage msg attrs
