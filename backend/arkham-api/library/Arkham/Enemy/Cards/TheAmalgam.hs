module Arkham.Enemy.Cards.TheAmalgam (theAmalgam, TheAmalgam (..)) where

import Arkham.Ability
import Arkham.Classes.HasQueue (withQueue_)
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Enemy.Runner (filterOutEnemyMessages)
import Arkham.I18n
import Arkham.Investigator.Projection ()
import Arkham.Key
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Scenarios.ThePitOfDespair.Helpers

newtype TheAmalgam = TheAmalgam EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theAmalgam :: EnemyCard TheAmalgam
theAmalgam = enemy TheAmalgam Cards.theAmalgam (3, Static 3, 2) (1, 1)

instance HasAbilities TheAmalgam where
  getAbilities (TheAmalgam x) =
    extend
      x
      [ restricted x 1 restriction
          $ freeReaction
          $ SkillTestResult #after You (whileEvading x) (SuccessResult $ atLeast 2)
      , forcedAbility x 2 $ EnemyEngaged #after (You <> InvestigatorWithAnyKey) (be x)
      , restricted x 3 depthsCriteria $ forced $ EnemyDefeated #when Anyone ByAny (be x)
      ]
   where
    restriction = if null (enemyKeys x) then Never else NoRestriction
    depthsCriteria = if x.placement == OutOfPlay TheDepths then Never else NoRestriction

instance RunMessage TheAmalgam where
  runMessage msg e@(TheAmalgam attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOneM iid $ withI18n do
        for_ (enemyKeys attrs) \key -> withVar "key" (String $ keyName key) do
          labeledI18n "takeControlOfKey" $ placeKey iid key
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> scenarioI18n do
      ikeys <- iid.keys
      chooseOneM iid do
        for_ ikeys \key -> withVar "key" (String $ keyName key) do
          labeledI18n "placeKeyOnTheAmalgam" $ placeKey attrs key
        labeledI18n "theAmalgamAttacksYou" $ initiateEnemyAttack attrs (attrs.ability 2) iid
      pure e
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      cancelEnemyDefeat attrs.id
      push $ PlaceEnemyOutOfPlay TheDepths attrs.id
      pure e
    PlaceEnemyOutOfPlay TheDepths eid | eid == attrs.id -> do
      lift $ withQueue_ $ mapMaybe (filterOutEnemyMessages attrs.id)
      pure
        . TheAmalgam
        $ attrs
        & (tokensL %~ mempty)
        & (placementL .~ OutOfPlay TheDepths)
        & (defeatedL .~ False)
        & (exhaustedL .~ False)
    _ -> TheAmalgam <$> liftRunMessage msg attrs
