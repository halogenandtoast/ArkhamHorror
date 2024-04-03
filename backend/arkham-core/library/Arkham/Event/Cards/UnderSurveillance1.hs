module Arkham.Event.Cards.UnderSurveillance1 (underSurveillance1, UnderSurveillance1 (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Discover
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
import Arkham.Helpers.Modifiers
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Placement
import Arkham.Prelude
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype UnderSurveillance1 = UnderSurveillance1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

underSurveillance1 :: EventCard UnderSurveillance1
underSurveillance1 = event UnderSurveillance1 Cards.underSurveillance1

instance HasAbilities UnderSurveillance1 where
  getAbilities (UnderSurveillance1 a) = case eventAttachedTarget a of
    Just (LocationTarget lid) ->
      [ restrictedAbility a 1 ControlsThis $ forced $ EnemyEnters #after (LocationWithId lid) NonEliteEnemy
      ]
    _ -> []

instance RunMessage UnderSurveillance1 where
  runMessage msg e@(UnderSurveillance1 attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      lid <- getJustLocation iid
      push $ PlaceEvent iid eid (AttachedToLocation lid)
      pure e
    UseCardAbility iid (isSource attrs -> True) 1 [windowType -> Window.EnemyEnters enemyId _] _ -> do
      case attrs.placement of
        AttachedToLocation lid -> do
          canDiscover <- getCanDiscoverClues NotInvestigate iid lid
          pushAll
            $ [toDiscardBy iid (attrs.ability 1) attrs, EnemyEvaded iid enemyId]
            <> [toMessage $ discover iid lid (attrs.ability 1) 1 | canDiscover]
            <> [nextPhaseModifier #upkeep (attrs.ability 1) enemyId DoesNotReadyDuringUpkeep]
        _ -> error "impossible"
      pure e
    _ -> UnderSurveillance1 <$> runMessage msg attrs
