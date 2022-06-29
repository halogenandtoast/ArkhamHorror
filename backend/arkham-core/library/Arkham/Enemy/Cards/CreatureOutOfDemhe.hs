module Arkham.Enemy.Cards.CreatureOutOfDemhe
  ( creatureOutOfDemhe
  , CreatureOutOfDemhe(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher

newtype CreatureOutOfDemhe = CreatureOutOfDemhe EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

creatureOutOfDemhe :: EnemyCard CreatureOutOfDemhe
creatureOutOfDemhe = enemyWith
  CreatureOutOfDemhe
  Cards.creatureOutOfDemhe
  (5, Static 4, 2)
  (1, 1)
  (spawnAtL ?~ LocationWithTitle "Depths of Demhe")

-- instance HasAbilities CreatureOutOfDemhe where
--   getAbilities (CreatureOutOfDemhe a) = withBaseAbilities a $
--     let
--       matcher = case enemyLocation a of
--                   Nothing -> Nowhere
--                   Just lid -> LocationOneOf [LocationWithId lid, ConnectedFrom (LocationWithId lid)]
--      in [mkAbility a 1 $ ForcedAbility $ FlipLocation Timing.When AnyOne matcher]

instance RunMessage CreatureOutOfDemhe where
  runMessage msg (CreatureOutOfDemhe attrs) =
    CreatureOutOfDemhe <$> runMessage msg attrs
