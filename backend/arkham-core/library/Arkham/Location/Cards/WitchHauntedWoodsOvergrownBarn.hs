module Arkham.Location.Cards.WitchHauntedWoodsOvergrownBarn (
  witchHauntedWoodsOvergrownBarn,
  WitchHauntedWoodsOvergrownBarn (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Attack
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype WitchHauntedWoodsOvergrownBarn = WitchHauntedWoodsOvergrownBarn LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

witchHauntedWoodsOvergrownBarn :: LocationCard WitchHauntedWoodsOvergrownBarn
witchHauntedWoodsOvergrownBarn =
  location
    WitchHauntedWoodsOvergrownBarn
    Cards.witchHauntedWoodsOvergrownBarn
    3
    (PerPlayer 1)

instance HasAbilities WitchHauntedWoodsOvergrownBarn where
  getAbilities (WitchHauntedWoodsOvergrownBarn a) =
    withBaseAbilities
      a
      [ restrictedAbility a 1 Here $
          ReactionAbility
            ( EnemyWouldSpawnAt
                AnyEnemy
                ( NotLocation (LocationWithId $ toId a)
                    <> LocationWithTitle "Witch-Haunted Woods"
                )
            )
            Free
      ]

instance RunMessage WitchHauntedWoodsOvergrownBarn where
  runMessage msg l@(WitchHauntedWoodsOvergrownBarn attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 [(windowType -> Window.EnemyWouldSpawnAt enemyId _)] _ ->
      do
        iids <- selectList $ investigatorAt $ toId attrs
        replaceMessageMatching
          ( \case
              When (EnemySpawn _ _ eid) -> eid == enemyId
              _ -> False
          )
          ( \case
              When (EnemySpawn _ _ eid) ->
                [When $ EnemySpawn Nothing (toId attrs) eid]
              _ -> error "bad match"
          )
        replaceMessageMatching
          ( \case
              EnemySpawn _ _ eid -> eid == enemyId
              _ -> False
          )
          ( \case
              EnemySpawn _ _ eid -> [EnemySpawn Nothing (toId attrs) eid]
              _ -> error "bad match"
          )
        replaceMessageMatching
          ( \case
              After (EnemySpawn _ _ eid) -> eid == enemyId
              _ -> False
          )
          ( \case
              After (EnemySpawn _ _ eid) ->
                After (EnemySpawn Nothing (toId attrs) eid)
                  : map (InitiateEnemyAttack . enemyAttack eid attrs) iids
              _ -> error "bad match"
          )
        pure l
    _ -> WitchHauntedWoodsOvergrownBarn <$> runMessage msg attrs
