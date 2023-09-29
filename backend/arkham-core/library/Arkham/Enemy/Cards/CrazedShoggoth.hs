module Arkham.Enemy.Cards.CrazedShoggoth (
  CrazedShoggoth (..),
  crazedShoggoth,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Trait

newtype CrazedShoggoth = CrazedShoggoth EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crazedShoggoth :: EnemyCard CrazedShoggoth
crazedShoggoth =
  enemyWith
    CrazedShoggoth
    Cards.crazedShoggoth
    (3, Static 6, 4)
    (2, 2)
    (spawnAtL ?~ SpawnLocation (NearestLocationToYou $ LocationWithTrait Altered))

instance HasAbilities CrazedShoggoth where
  getAbilities (CrazedShoggoth attrs) =
    withBaseAbilities
      attrs
      [ mkAbility attrs 1
          $ ForcedAbility
          $ InvestigatorDefeated
            Timing.When
            (BySource $ SourceIsEnemyAttack $ EnemyWithId $ toId attrs)
            You
      ]

instance RunMessage CrazedShoggoth where
  runMessage msg e@(CrazedShoggoth attrs) = case msg of
    UseCardAbility iid source 1 _ _
      | isSource attrs source ->
          e <$ push (InvestigatorKilled source iid)
    _ -> CrazedShoggoth <$> runMessage msg attrs
