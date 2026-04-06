module Arkham.Event.Events.GuerrillaTactics2 (guerrillaTactics2) where

import Arkham.Action (Action)
import Arkham.Criteria
import Arkham.Evade.Types
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Fight.Types
import Arkham.ForMovement
import Arkham.Helpers.SkillTest (withSkillTestTargetedEnemy)
import Arkham.Matcher
import Arkham.Modifier

newtype GuerrillaTactics2 = GuerrillaTactics2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

guerrillaTactics2 :: EventCard GuerrillaTactics2
guerrillaTactics2 = event GuerrillaTactics2 Cards.guerrillaTactics2

connectingEnemyMatcher :: EnemyMatcher
connectingEnemyMatcher = fightOverride $ EnemyAt (orConnected NotForMovement YourLocation)

instance RunMessage GuerrillaTactics2 where
  runMessage msg e@(GuerrillaTactics2 attrs) = runQueueT $ case msg of
    BeforePlayEvent iid eid acId | eid == toId attrs -> do
      chooseOneM iid do
        labeled "Fight (combat)" do
          pushAll
            [ UpdateEventMeta eid (toJSON (#fight :: Action))
            , SetActiveCostChosenAction acId #fight
            , CreatedCost acId
            ]
        labeled "Evade (agility)" do
          pushAll
            [ UpdateEventMeta eid (toJSON (#evade :: Action))
            , SetActiveCostChosenAction acId #evade
            , CreatedCost acId
            ]
      pure e
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      case getEventMeta @Action attrs of
        Just action | action == #fight -> do
          skillTestModifier sid attrs iid (SkillModifier #combat 1)
          chooseFightEnemyEdit sid iid attrs \cf ->
            cf
              { chooseFightEnemyMatcher = connectingEnemyMatcher
              , chooseFightOverride = True
              }
        Just action | action == #evade -> do
          skillTestModifier sid attrs iid (SkillModifier #agility 1)
          chooseEvadeEnemyEdit sid iid attrs \ce ->
            ce
              { chooseEvadeEnemyMatcher = evadeOverride (EnemyAt (orConnected NotForMovement YourLocation))
              , chooseEvadeOverride = True
              }
        _ -> pure ()
      pure e
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      withSkillTestTargetedEnemy $ nonAttackEnemyDamage (Just iid) attrs 1
      pure e
    _ -> GuerrillaTactics2 <$> liftRunMessage msg attrs
