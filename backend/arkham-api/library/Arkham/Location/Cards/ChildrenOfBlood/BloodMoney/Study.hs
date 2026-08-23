module Arkham.Location.Cards.ChildrenOfBlood.BloodMoney.Study (study) where

import Arkham.Ability
import Arkham.Location.CardDefs.ChildrenOfBlood.BloodMoney qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype Study = Study LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

study :: LocationCard Study
study = symbolLabel $ location Study Cards.study 3 (PerPlayer 1)

instance HasAbilities Study where
  getAbilities (Study a) =
    extendRevealed1 a
      $ mkAbility a 1
      $ forced
      $ SkillTestResult #after You (whileInvestigating a) #failure

instance RunMessage Study where
  runMessage msg l@(Study attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawEncounterCard iid (attrs.ability 1)
      pure l
    _ -> Study <$> liftRunMessage msg attrs
