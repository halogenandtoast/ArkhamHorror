module Arkham.Types.Enemy.Cards.CrazedShoggoth
  ( CrazedShoggoth(..)
  , crazedShoggoth
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (InvestigatorDefeated)
import Arkham.Types.Source
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Trait

newtype CrazedShoggoth = CrazedShoggoth EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crazedShoggoth :: EnemyCard CrazedShoggoth
crazedShoggoth = enemyWith
  CrazedShoggoth
  Cards.crazedShoggoth
  (3, Static 6, 4)
  (2, 2)
  (spawnAtL ?~ NearestLocationToYou (LocationWithTrait Altered))

instance HasAbilities env CrazedShoggoth where
  getAbilities iid window (CrazedShoggoth attrs) =
    withBaseAbilities iid window attrs $ pure
      [ mkAbility attrs 1 $ ForcedAbility $ InvestigatorDefeated
          Timing.When
          (SourceIs $ AttackSource $ toId attrs)
          You
      ]

instance EnemyAttrsRunMessage env => RunMessage env CrazedShoggoth where
  runMessage msg e@(CrazedShoggoth attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      e <$ push (InvestigatorKilled source iid)
    _ -> CrazedShoggoth <$> runMessage msg attrs
