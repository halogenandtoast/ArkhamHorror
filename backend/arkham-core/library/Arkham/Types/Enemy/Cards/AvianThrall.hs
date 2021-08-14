module Arkham.Types.Enemy.Cards.AvianThrall
  ( AvianThrall(..)
  , avianThrall
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Game.Helpers
import Arkham.Types.Id
import Arkham.Types.Modifier
import Arkham.Types.Prey
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Trait

newtype AvianThrall = AvianThrall EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasActions)

avianThrall :: EnemyCard AvianThrall
avianThrall = enemyWith
  AvianThrall
  Cards.avianThrall
  (5, Static 4, 3)
  (1, 1)
  (preyL .~ LowestSkill SkillIntellect)

instance HasSet Trait env AssetId => HasModifiersFor env AvianThrall where
  getModifiersFor (AssetSource aid) target (AvianThrall attrs)
    | isTarget attrs target = do
      traits <- getSet aid
      pure $ toModifiers
        attrs
        [ EnemyFight (-3) | any (`elem` [Ranged, Firearm, Spell]) traits ]
  getModifiersFor _ _ _ = pure []

instance EnemyRunner env => RunMessage env AvianThrall where
  runMessage msg (AvianThrall attrs) = AvianThrall <$> runMessage msg attrs
