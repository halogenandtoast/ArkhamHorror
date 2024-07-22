module Arkham.Enemy.Cards.LodgeNeophyte (
  lodgeNeophyte,
  LodgeNeophyte (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype LodgeNeophyte = LodgeNeophyte EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lodgeNeophyte :: EnemyCard LodgeNeophyte
lodgeNeophyte =
  enemyWith
    LodgeNeophyte
    Cards.lodgeNeophyte
    (3, Static 1, 2)
    (0, 1)
    (spawnAtL ?~ SpawnAt EmptyLocation)

instance HasAbilities LodgeNeophyte where
  getAbilities (LodgeNeophyte a) =
    withBaseAbilities
      a
      [ restrictedAbility a 1 CanPlaceDoomOnThis
          $ ForcedAbility
          $ EnemySpawns Timing.After Anywhere
          $ EnemyWithId
          $ toId a
      , restrictedAbility a 2 OnSameLocation
          $ ActionAbility [Action.Parley]
          $ ActionCost 1
      ]

instance RunMessage LodgeNeophyte where
  runMessage msg e@(LodgeNeophyte attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      e <$ push (PlaceDoom (toAbilitySource attrs 1) (toTarget attrs) 1)
    UseCardAbility iid source 2 _ _ | isSource attrs source -> do
      sid <- getRandom
      push $ parley sid iid attrs attrs SkillWillpower (Fixed 2)
      pure e
    PassedSkillTest _ _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ ->
      do
        push $ RemoveAllDoom (toAbilitySource attrs 2) (toTarget attrs)
        pure e
    _ -> LodgeNeophyte <$> runMessage msg attrs
