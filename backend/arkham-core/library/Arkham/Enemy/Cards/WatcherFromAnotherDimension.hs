module Arkham.Enemy.Cards.WatcherFromAnotherDimension (
  watcherFromAnotherDimension,
  WatcherFromAnotherDimension (..),
)
where

import Arkham.Prelude

import Arkham.Attack
import Arkham.Classes
import Arkham.Constants
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Placement

newtype WatcherFromAnotherDimension = WatcherFromAnotherDimension EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

watcherFromAnotherDimension :: EnemyCard WatcherFromAnotherDimension
watcherFromAnotherDimension =
  enemyWith
    WatcherFromAnotherDimension
    Cards.watcherFromAnotherDimension
    (5, Static 2, 5)
    (3, 0)
    (spawnAtL ?~ NoSpawn)

instance HasAbilities WatcherFromAnotherDimension where
  getAbilities (WatcherFromAnotherDimension a) = case enemyPlacement a of
    StillInHand iid ->
      [ restrictedAbility
          a
          AbilityAttack
          ( exists (You <> InvestigatorWithId iid)
              <> EnemyCriteria (ThisEnemy $ EnemyWithoutModifier CannotBeAttacked)
              <> CanAttack
          )
          $ ActionAbility (Just #fight) (ActionCost 1)
      , restrictedAbility
          a
          AbilityEvade
          (exists (You <> InvestigatorWithId iid))
          $ ActionAbility (Just #evade) (ActionCost 1)
      , mkAbility a 1 $ ForcedAbility $ Matcher.DeckHasNoCards #when (You <> InvestigatorWithId iid)
      ]
    _ -> getAbilities a

instance RunMessage WatcherFromAnotherDimension where
  runMessage msg e@(WatcherFromAnotherDimension attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      pushAll [PlaceEnemy (toId e) (StillInHand iid)]
      pure e
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ InitiateEnemyAttack $ enemyAttack (toId attrs) attrs iid
      pure e
    Successful (action, _) _ _ (isTarget attrs -> True) _ | action `elem` [#fight, #evade] -> do
      case enemyPlacement attrs of
        StillInHand _ -> do
          push $ Discard (toSource attrs) (toTarget attrs)
          pure e
        _ -> WatcherFromAnotherDimension <$> runMessage msg attrs
    FailedSkillTest iid (Just action) _ (Initiator (isTarget attrs -> True)) _ _ | action `elem` [#fight, #evade] -> do
      case enemyPlacement attrs of
        StillInHand _ -> do
          push $ EnemySpawnAtLocationMatching (Just iid) (locationWithInvestigator iid) (toId attrs)
          pure e
        _ -> WatcherFromAnotherDimension <$> runMessage msg attrs
    _ -> WatcherFromAnotherDimension <$> runMessage msg attrs
