module Arkham.Enemy.Cards.WatcherFromAnotherDimension (
  watcherFromAnotherDimension,
  WatcherFromAnotherDimension (..),
)
where

import Arkham.Ability
import Arkham.Constants
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Enemy
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Modifier
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
  getAbilities (WatcherFromAnotherDimension a) = case a.placement of
    StillInHand iid ->
      [ restricted
          a
          AbilityAttack
          ( youExist (InvestigatorWithId iid)
              <> thisEnemy (EnemyWithoutModifier CannotBeAttacked)
              <> CanAttack
          )
          fightAction_
      , restricted a AbilityEvade (youExist $ InvestigatorWithId iid) evadeAction_
      , mkAbility a 1 $ forced $ Matcher.DeckHasNoCards #when (You <> InvestigatorWithId iid)
      ]
    _ -> getAbilities a

instance RunMessage WatcherFromAnotherDimension where
  runMessage msg e@(WatcherFromAnotherDimension attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      place attrs (StillInHand iid)
      pure e
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      initiateEnemyAttack attrs attrs iid
      pure e
    Successful (action, (isTarget attrs -> True)) iid _ _ _ | action `elem` [#fight, #evade] -> do
      case attrs.placement of
        StillInHand _ -> do
          toDiscardBy iid (toSource attrs) attrs
          pure e
        _ -> WatcherFromAnotherDimension <$> liftRunMessage msg attrs
    FailedSkillTest iid (Just action) _ (Initiator (isActionTarget attrs -> True)) _ _ | action `elem` [#fight, #evade] -> do
      case attrs.placement of
        StillInHand _ -> do
          push $ EnemySpawnAtLocationMatching (Just iid) (locationWithInvestigator iid) (toId attrs)
          pure e
        _ -> WatcherFromAnotherDimension <$> liftRunMessage msg attrs
    _ -> WatcherFromAnotherDimension <$> liftRunMessage msg attrs
