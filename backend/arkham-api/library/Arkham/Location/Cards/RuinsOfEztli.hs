module Arkham.Location.Cards.RuinsOfEztli (ruinsOfEztli, RuinsOfEztli (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype RuinsOfEztli = RuinsOfEztli LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruinsOfEztli :: LocationCard RuinsOfEztli
ruinsOfEztli = location RuinsOfEztli Cards.ruinsOfEztli 3 (PerPlayer 2)

instance HasAbilities RuinsOfEztli where
  getAbilities (RuinsOfEztli attrs) =
    withBaseAbilities
      attrs
      [mkAbility attrs 1 $ forced $ SkillTestResult #after You (WhileInvestigating $ be attrs) #failure]

instance RunMessage RuinsOfEztli where
  runMessage msg l@(RuinsOfEztli attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ drawEncounterCard iid attrs
      pure l
    _ -> RuinsOfEztli <$> runMessage msg attrs
