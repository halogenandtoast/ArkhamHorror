module Arkham.Enemy.Cards.CreatureOutOfDemhe (
  creatureOutOfDemhe,
  CreatureOutOfDemhe (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Attack
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Timing qualified as Timing
import Arkham.Window
import Arkham.Window qualified as Window

newtype CreatureOutOfDemhe = CreatureOutOfDemhe EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

creatureOutOfDemhe :: EnemyCard CreatureOutOfDemhe
creatureOutOfDemhe =
  enemyWith
    CreatureOutOfDemhe
    Cards.creatureOutOfDemhe
    (5, Static 4, 2)
    (1, 1)
    (spawnAtL ?~ SpawnAt (LocationWithTitle "Depths of Demhe"))

instance HasAbilities CreatureOutOfDemhe where
  getAbilities (CreatureOutOfDemhe a) =
    withBaseAbilities a
      $ [ mkAbility a 1
            $ ForcedAbility
            $ Matcher.FlipLocation
              Timing.When
              Anyone
              (LocationMatchAny [LocationOfThis, ConnectedFrom LocationOfThis])
        ]

instance RunMessage CreatureOutOfDemhe where
  runMessage msg e@(CreatureOutOfDemhe attrs) = case msg of
    UseCardAbility _ source 1 [(windowType -> Window.FlipLocation _ lid)] _
      | isSource attrs source -> do
          iids <- selectList $ InvestigatorAt $ LocationWithId lid
          pushAll $ map (InitiateEnemyAttack . enemyAttack (toId attrs) attrs) iids
          pure e
    _ -> CreatureOutOfDemhe <$> runMessage msg attrs
