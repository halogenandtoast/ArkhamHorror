module Arkham.Enemy.Cards.LodgeJailor (
  lodgeJailor,
  LodgeJailor (..),
)
where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Key
import Arkham.Matcher
import Arkham.Scenarios.ForTheGreaterGood.Helpers
import Arkham.SkillType
import Arkham.Timing qualified as Timing
import Arkham.Trait (Trait (Sanctum))

newtype LodgeJailor = LodgeJailor EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

lodgeJailor :: EnemyCard LodgeJailor
lodgeJailor =
  enemyWith
    LodgeJailor
    Cards.lodgeJailor
    (2, Static 3, 3)
    (0, 2)
    (spawnAtL ?~ SpawnAt (LocationWithTrait Sanctum))

instance HasAbilities LodgeJailor where
  getAbilities (LodgeJailor attrs) =
    withBaseAbilities
      attrs
      [ mkAbility attrs 1 $ ForcedAbility $ EnemySpawns Timing.After Anywhere $ EnemyWithId $ toId attrs
      , restrictedAbility attrs 2 OnSameLocation $ ActionAbility [Action.Parley] (ActionCost 1)
      ]

instance RunMessage LodgeJailor where
  runMessage msg e@(LodgeJailor attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      mKey <- getRandomKey
      pushAll
        $ PlaceDoom (toAbilitySource attrs 1) (toTarget attrs) 2
        : [PlaceKey (toTarget attrs) k | k <- maybeToList mKey]
      pure e
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      push $ parley iid (toAbilitySource attrs 2) attrs SkillIntellect 3
      pure e
    PassedSkillTest iid _ (isAbilitySource attrs 2 -> True) SkillTestInitiatorTarget {} _ _ -> do
      let
        hasKey = notNull $ enemyKeys attrs
        hasDoom = enemyDoom attrs > 0
      player <- getPlayer iid
      when (hasKey || hasDoom) $ do
        push
          $ chooseOrRunOne player
          $ [Label "Remove 1 Doom" [RemoveDoom (toAbilitySource attrs 2) (toTarget attrs) 1] | hasDoom]
          <> [ Label ("Take control of the " <> keyName k <> " key") [PlaceKey (toTarget iid) k]
             | k <- setToList $ enemyKeys attrs
             ]
      pure e
    _ -> LodgeJailor <$> runMessage msg attrs
