module Arkham.Location.Cards.WitchHauntedWoodsOvergrownBarn
  ( witchHauntedWoodsOvergrownBarn
  , WitchHauntedWoodsOvergrownBarn(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window

newtype WitchHauntedWoodsOvergrownBarn = WitchHauntedWoodsOvergrownBarn LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

witchHauntedWoodsOvergrownBarn :: LocationCard WitchHauntedWoodsOvergrownBarn
witchHauntedWoodsOvergrownBarn = location
  WitchHauntedWoodsOvergrownBarn
  Cards.witchHauntedWoodsOvergrownBarn
  3
  (PerPlayer 1)

instance HasAbilities WitchHauntedWoodsOvergrownBarn where
  getAbilities (WitchHauntedWoodsOvergrownBarn a) = withBaseAbilities
    a
    [ restrictedAbility a 1 Here $ ReactionAbility
        (EnemyWouldSpawnAt AnyEnemy (NotLocation $ LocationWithId $ toId a))
        Free
    ]

instance RunMessage WitchHauntedWoodsOvergrownBarn where
  runMessage msg l@(WitchHauntedWoodsOvergrownBarn attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 [Window _ (Window.EnemyWouldSpawnAt enemyId _)] _
      -> do
        replaceMessageMatching
          (\case
            When (EnemySpawn _ _ eid) -> eid == enemyId
            _ -> False
          )
          (\case
            When (EnemySpawn mId _ eid) ->
              [When $ EnemySpawn mId (toId attrs) eid]
            _ -> error "bad match"
          )
        replaceMessageMatching
          (\case
            EnemySpawn _ _ eid -> eid == enemyId
            _ -> False
          )
          (\case
            EnemySpawn mId _ eid -> [EnemySpawn mId (toId attrs) eid]
            _ -> error "bad match"
          )
        replaceMessageMatching
          (\case
            After (EnemySpawn _ _ eid) -> eid == enemyId
            _ -> False
          )
          (\case
            After (EnemySpawn mId _ eid) ->
              [After $ EnemySpawn mId (toId attrs) eid]
            _ -> error "bad match"
          )
        pure l
    _ -> WitchHauntedWoodsOvergrownBarn <$> runMessage msg attrs
