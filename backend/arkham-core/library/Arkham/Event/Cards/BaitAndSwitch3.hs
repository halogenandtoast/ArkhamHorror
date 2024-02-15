module Arkham.Event.Cards.BaitAndSwitch3 (
  baitAndSwitch3,
  BaitAndSwitch3,
) where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Criteria
import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards (baitAndSwitch3)
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
import Arkham.Helpers.Modifiers
import Arkham.Id
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Movement
import Arkham.Projection
import Arkham.SkillType

newtype Metadata = Metadata {choice :: Maybe Int}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype BaitAndSwitch3 = BaitAndSwitch3 (EventAttrs `With` Metadata)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

baitAndSwitch3 :: EventCard BaitAndSwitch3
baitAndSwitch3 =
  event (BaitAndSwitch3 . (`with` Metadata Nothing)) Cards.baitAndSwitch3

baitAndSwitch3Matcher :: InvestigatorId -> EventAttrs -> Int -> EnemyMatcher
baitAndSwitch3Matcher iid attrs = \case
  1 -> CanEvadeEnemy (toSource attrs) <> EnemyAt (locationWithInvestigator iid)
  2 ->
    CanEvadeEnemyWithOverride
      $ CriteriaOverride
      $ EnemyCriteria
      $ ThisEnemy
      $ EnemyAt ConnectedLocation
      <> NonEliteEnemy
  _ -> error "Invalid choice"

instance RunMessage BaitAndSwitch3 where
  runMessage msg e@(BaitAndSwitch3 (attrs@EventAttrs {..} `With` meta)) =
    case msg of
      InvestigatorPlayEvent iid eid mTarget ws _ | eid == eventId -> do
        choice1Enemies <- select (baitAndSwitch3Matcher iid attrs 1)
        choice2Enemies <- select (baitAndSwitch3Matcher iid attrs 2)
        player <- getPlayer iid
        push
          $ chooseOrRunOne player
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
      ResolveEventChoice iid eid n _ _ | eid == eventId -> do
        push
          $ ChooseEvadeEnemy
            iid
            (toSource attrs)
            (Just $ toTarget attrs)
            SkillAgility
            AnyEnemy
            False
        when (n == 2)
          $ push
          $ skillTestModifier
            attrs
            iid
            ( EnemyEvadeActionCriteria
                $ CriteriaOverride
                $ EnemyCriteria
                  (ThisEnemy $ NonEliteEnemy <> EnemyAt ConnectedLocation)
            )

        pure $ BaitAndSwitch3 (attrs `with` Metadata (Just n))
      Successful (Action.Evade, EnemyTarget eid) iid _ target _
        | isTarget attrs target -> do
            nonElite <- elem eid <$> select NonEliteEnemy
            case choice meta of
              Just 1 ->
                pushAll
                  $ EnemyEvaded iid eid
                  : [WillMoveEnemy eid msg | nonElite]
              Just 2 -> do
                lid <- getJustLocation iid
                enemyLocation <- fieldJust EnemyLocation eid
                pushAll
                  [ EnemyEvaded iid eid
                  , EnemyMove eid lid
                  , Move $ move attrs iid enemyLocation
                  ]
              _ -> error "Missing event choice"
            pure e
      WillMoveEnemy enemyId (Successful (Action.Evade, _) iid _ target _)
        | isTarget attrs target -> do
            choices <- select ConnectedLocation
            player <- getPlayer iid
            let
              enemyMoveChoices =
                chooseOne
                  player
                  [ targetLabel choice [EnemyMove enemyId choice]
                  | choice <- choices
                  ]
            insertAfterMatching
              [enemyMoveChoices]
              \case
                AfterEvadeEnemy {} -> True
                _ -> False
            pure e
      _ -> BaitAndSwitch3 . (`with` meta) <$> runMessage msg attrs
