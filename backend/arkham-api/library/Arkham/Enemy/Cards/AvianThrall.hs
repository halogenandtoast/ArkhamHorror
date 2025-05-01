module Arkham.Enemy.Cards.AvianThrall (avianThrall) where

import Arkham.Action qualified as Action
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest
import Arkham.Helpers.Source
import Arkham.Matcher
import Arkham.Modifier qualified as Modifier
import Arkham.Trait

newtype AvianThrall = AvianThrall EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

avianThrall :: EnemyCard AvianThrall
avianThrall =
  enemy AvianThrall Cards.avianThrall (5, Static 4, 3) (1, 1)
    & setPrey (InvestigatorWithLowestSkill #intellect UneliminatedInvestigator)

instance HasModifiersFor AvianThrall where
  getModifiersFor (AvianThrall a) = modifySelfMaybe a do
    source <- MaybeT getSkillTestSource
    Action.Fight <- MaybeT getSkillTestAction
    traits <- lift $ sourceTraits source
    guard $ any (`elem` [Ranged, Firearm, Spell]) traits
    pure [Modifier.EnemyFight (-3)]

instance RunMessage AvianThrall where
  runMessage msg (AvianThrall attrs) = AvianThrall <$> runMessage msg attrs
