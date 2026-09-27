module Arkham.Event.Events.BaitAndSwitch3 (baitAndSwitch3) where

import Arkham.Action qualified as Action
import Arkham.Criteria
import Arkham.Enemy.Types (Field (..))
import Arkham.Evade
import Arkham.Event.Cards qualified as Cards (baitAndSwitch3)
import Arkham.Event.Import.Lifted
import Arkham.ForMovement
import Arkham.Helpers.Investigator hiding (setMeta)
import Arkham.I18n
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Message.Lifted.Move
import Arkham.Projection

newtype BaitAndSwitch3 = BaitAndSwitch3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

baitAndSwitch3 :: EventCard BaitAndSwitch3
baitAndSwitch3 = event BaitAndSwitch3 Cards.baitAndSwitch3

override :: CriteriaOverride
override =
  CriteriaOverride
    $ EnemyCriteria
    $ ThisEnemy
    $ EnemyAt (ConnectedLocation NotForMovement)
    <> NonEliteEnemy

baitAndSwitch3Matcher :: InvestigatorId -> EventAttrs -> Int -> EnemyMatcher
baitAndSwitch3Matcher iid attrs = \case
  1 -> CanEvadeEnemy (toSource attrs) <> EnemyAt (locationWithInvestigator iid)
  2 -> CanEvadeEnemyWithOverride override
  _ -> error "Invalid choice"

instance RunMessage BaitAndSwitch3 where
  runMessage msg e@(BaitAndSwitch3 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      canEvadeHere <- selectAny $ baitAndSwitch3Matcher iid attrs 1
      canEvadeConnecting <- selectAny $ baitAndSwitch3Matcher iid attrs 2
      chooseOrRunOneM iid $ cardI18n $ scope "baitAndSwitch" do
        when canEvadeHere $ labeled "evadeAndMove" $ doStep 1 msg
        when canEvadeConnecting $ labeled "evadeAndSwitch" $ doStep 2 msg
      pure e
    DoStep n (PlayThisEvent iid (is attrs -> True)) -> do
      sid <- getRandom
      -- The override has to ride on the ChooseEvade itself: as a skill test
      -- window modifier it isn't visible until the test exists, which is after
      -- ChooseEvadeEnemy has already picked the eligible enemies.
      pushM $ setTarget attrs <$> case n of
        2 -> mkChooseEvadeMatch sid iid attrs (baitAndSwitch3Matcher iid attrs 2)
        _ -> mkChooseEvade sid iid attrs
      pure . BaitAndSwitch3 $ setMeta n attrs
    Successful (Action.Evade, EnemyTarget eid) iid _ target _ | isTarget attrs target -> do
      nonElite <- eid <=~> NonEliteEnemy
      case getEventMeta @Int attrs of
        Just 1 -> pushAll $ EnemyEvaded iid eid : [WillMoveEnemy eid msg | nonElite]
        Just 2 -> do
          lid <- getJustLocation iid
          enemyLocation <- fieldJust EnemyLocation eid
          push $ EnemyEvaded iid eid
          push $ EnemyMove eid lid
          moveTo attrs iid enemyLocation
        _ -> error "Missing event choice"
      pure e
    WillMoveEnemy enemyId (Successful (Action.Evade, _) iid _ target _) | isTarget attrs target -> do
      choices <-
        select
          $ ConnectedFrom NotForMovement (locationWithInvestigator iid)
          <> LocationCanBeEnteredBy enemyId
      enemyMoveChoices <- capture $ chooseOne iid $ targetLabels choices $ only . EnemyMove enemyId
      insertAfterMatching enemyMoveChoices \case
        AfterEvadeEnemy {} -> True
        _ -> False
      pure e
    _ -> BaitAndSwitch3 <$> liftRunMessage msg attrs
