module Arkham.Event.Events.GuerrillaTactics (guerrillaTactics) where

import Arkham.Action (Action)
import Arkham.Criteria
import Arkham.Evade.Types
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Fight.Types
import Arkham.ForMovement
import Arkham.Matcher
import Arkham.Modifier

newtype GuerrillaTactics = GuerrillaTactics EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

guerrillaTactics :: EventCard GuerrillaTactics
guerrillaTactics = event GuerrillaTactics Cards.guerrillaTactics

connectingEnemyMatcher :: EnemyMatcher
connectingEnemyMatcher = fightOverride $ EnemyAt (orConnected NotForMovement YourLocation)

instance RunMessage GuerrillaTactics where
  runMessage msg e@(GuerrillaTactics attrs) = runQueueT $ case msg of
    BeforePlayEvent iid eid acId | eid == toId attrs -> do
      chooseOneM iid do
        labeled "Fight ({combat})" do
          pushAll
            [ UpdateEventMeta eid (toJSON (#fight :: Action))
            , SetActiveCostChosenAction acId #fight
            , CreatedCost acId
            ]
        labeled "Evade ({agility})" do
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
    _ -> GuerrillaTactics <$> liftRunMessage msg attrs
