module Arkham.Location.Cards.RuinsOfEztli (ruinsOfEztli) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype RuinsOfEztli = RuinsOfEztli LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruinsOfEztli :: LocationCard RuinsOfEztli
ruinsOfEztli = symbolLabel $ location RuinsOfEztli Cards.ruinsOfEztli 3 (PerPlayer 2)

instance HasAbilities RuinsOfEztli where
  getAbilities (RuinsOfEztli attrs) =
    extendRevealed1 attrs
      $ mkAbility attrs 1
      $ forced
      $ SkillTestResult #after You (WhileInvestigating $ be attrs) #failure

instance RunMessage RuinsOfEztli where
  runMessage msg l@(RuinsOfEztli attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawEncounterCard iid attrs
      pure l
    _ -> RuinsOfEztli <$> liftRunMessage msg attrs
