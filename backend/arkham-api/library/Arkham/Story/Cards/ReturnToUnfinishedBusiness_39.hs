module Arkham.Story.Cards.ReturnToUnfinishedBusiness_39 (returnToUnfinishedBusiness_39) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Evade.Types
import Arkham.Helpers.Investigator (getSkillValue)
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Window qualified as Window

newtype ReturnToUnfinishedBusiness_39 = ReturnToUnfinishedBusiness_39 StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToUnfinishedBusiness_39 :: StoryCard ReturnToUnfinishedBusiness_39
returnToUnfinishedBusiness_39 = story ReturnToUnfinishedBusiness_39 Cards.returnToUnfinishedBusiness_39

instance HasAbilities ReturnToUnfinishedBusiness_39 where
  getAbilities (ReturnToUnfinishedBusiness_39 x) = case x.placement of
    InThreatArea _ ->
      [ restricted x 1 (InThreatAreaOf You <> exists (enemyIs Enemies.theSpectralWatcher <> ExhaustedEnemy))
          $ forced
          $ PhaseBegins #when #enemy
      , restricted
          x
          2
          (InThreatAreaOf You <> exists (CanEvadeEnemy (toSource x) <> enemyIs Enemies.theSpectralWatcher))
          evadeAction_
      ]
    _ -> []

instance RunMessage ReturnToUnfinishedBusiness_39 where
  runMessage msg s@(ReturnToUnfinishedBusiness_39 attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      pure
        . ReturnToUnfinishedBusiness_39
        $ attrs
        & (placementL .~ InThreatArea iid)
        & (removeAfterResolutionL .~ False)
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      selectOne (enemyIs Enemies.theSpectralWatcher) >>= traverse_ readyThis
      pure s
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      choices <- mins <$> traverse (traverseToSnd (`getSkillValue` iid)) [minBound .. maxBound]
      chooseOrRunOneM iid do
        for_ choices \kind -> skillLabeled kind $ forSkillType kind msg
      pure s
    ForSkillType kind (UseThisAbility iid (isSource attrs -> True) 2) -> do
      let source = attrs.ability 2
      sid <- getRandom
      chooseEvadeEnemyEdit sid iid source \c ->
        c
          { chooseEvadeEnemyMatcher = enemyIs Enemies.theSpectralWatcher
          , chooseEvadeTarget = Just (toTarget attrs)
          , chooseEvadeSkillType = kind
          }
      pure s
    Successful (Action.Evade, EnemyTarget enemyId) iid _ (isTarget attrs -> True) _ -> do
      push $ EnemyEvaded iid enemyId
      let card = lookupCard Enemies.returnToHeretic_39 (toCardId attrs)
      batched \_ -> do
        checkWhen $ Window.ScenarioEvent "wouldBanish" (Just iid) (toJSON card)
        send $ format card <> " is \"banished\""
        addToVictory iid attrs
      pure s
    _ -> ReturnToUnfinishedBusiness_39 <$> liftRunMessage msg attrs
