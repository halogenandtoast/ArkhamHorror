module Arkham.Event.Cards.BaitAndSwitch3 (baitAndSwitch3, BaitAndSwitch3) where

import Arkham.Action qualified as Action
import Arkham.Classes.HasQueue (evalQueueT)
import Arkham.Criteria
import Arkham.Enemy.Types (Field (..))
import Arkham.Evade
import Arkham.Event.Cards qualified as Cards (baitAndSwitch3)
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Investigator
import Arkham.Id
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Modifier
import Arkham.Movement
import Arkham.Projection

newtype Metadata = Metadata {choice :: Maybe Int}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype BaitAndSwitch3 = BaitAndSwitch3 (EventAttrs `With` Metadata)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

baitAndSwitch3 :: EventCard BaitAndSwitch3
baitAndSwitch3 = event (BaitAndSwitch3 . (`with` Metadata Nothing)) Cards.baitAndSwitch3

override :: CriteriaOverride
override = CriteriaOverride $ EnemyCriteria $ ThisEnemy $ EnemyAt ConnectedLocation <> NonEliteEnemy

baitAndSwitch3Matcher :: InvestigatorId -> EventAttrs -> Int -> EnemyMatcher
baitAndSwitch3Matcher iid attrs = \case
  1 -> CanEvadeEnemy (toSource attrs) <> EnemyAt (locationWithInvestigator iid)
  2 -> CanEvadeEnemyWithOverride override
  _ -> error "Invalid choice"

instance RunMessage BaitAndSwitch3 where
  runMessage msg e@(BaitAndSwitch3 (attrs `With` meta)) = runQueueT $ case msg of
    InvestigatorPlayEvent iid eid mTarget ws _ | eid == attrs.id -> do
      choice1Enemies <- select $ baitAndSwitch3Matcher iid attrs 1
      choice2Enemies <- select $ baitAndSwitch3Matcher iid attrs 2
      chooseOrRunOne iid
        $ [ Label
            "Evade. If you succeed and the enemy is non-Elite, evade the enemy and move it to a connecting location."
            [ResolveEventChoice iid eid 1 mTarget ws]
          | notNull choice1Enemies
          ]
        <> [ Label
            "Evade. Use only on a non-Elite enemy at a connecting location. If you succeed, evade that enemy and switch locations with it."
            [ResolveEventChoice iid eid 2 mTarget ws]
           | notNull choice2Enemies
           ]
      pure e
    ResolveEventChoice iid eid n _ _ | eid == attrs.id -> do
      sid <- getRandom
      pushM $ setTarget attrs <$> mkChooseEvade sid iid attrs
      when (n == 2) $ skillTestModifier sid attrs iid $ EnemyEvadeActionCriteria override
      pure $ BaitAndSwitch3 (attrs `with` Metadata (Just n))
    Successful (Action.Evade, EnemyTarget eid) iid _ target _ | isTarget attrs target -> do
      nonElite <- eid <=~> NonEliteEnemy
      case choice meta of
        Just 1 -> pushAll $ EnemyEvaded iid eid : [WillMoveEnemy eid msg | nonElite]
        Just 2 -> do
          lid <- getJustLocation iid
          enemyLocation <- fieldJust EnemyLocation eid
          pushAll [EnemyEvaded iid eid, EnemyMove eid lid, Move $ move attrs iid enemyLocation]
        _ -> error "Missing event choice"
      pure e
    WillMoveEnemy enemyId (Successful (Action.Evade, _) iid _ target _) | isTarget attrs target -> do
      choices <- select $ ConnectedFrom (locationWithInvestigator iid) <> LocationCanBeEnteredBy enemyId
      enemyMoveChoices <- evalQueueT $ chooseOne iid $ targetLabels choices $ only . EnemyMove enemyId
      insertAfterMatching enemyMoveChoices \case
        AfterEvadeEnemy {} -> True
        _ -> False
      pure e
    _ -> BaitAndSwitch3 . (`with` meta) <$> liftRunMessage msg attrs
