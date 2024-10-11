module Arkham.Event.Events.UnderSurveillance1 (underSurveillance1, UnderSurveillance1 (..)) where

import Arkham.Ability
import Arkham.Discover
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Investigator
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Modifier
import Arkham.Placement
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype UnderSurveillance1 = UnderSurveillance1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

underSurveillance1 :: EventCard UnderSurveillance1
underSurveillance1 = event UnderSurveillance1 Cards.underSurveillance1

instance HasAbilities UnderSurveillance1 where
  getAbilities (UnderSurveillance1 a) = case a.attachedTo of
    Just (LocationTarget lid) ->
      [ restricted a 1 ControlsThis $ forced $ EnemyEnters #after (LocationWithId lid) NonEliteEnemy
      ]
    _ -> []

instance RunMessage UnderSurveillance1 where
  runMessage msg e@(UnderSurveillance1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == attrs.id -> do
      withLocationOf iid (place attrs . AttachedToLocation)
      pure e
    UseCardAbility iid (isSource attrs -> True) 1 [windowType -> Window.EnemyEnters enemyId _] _ -> do
      case attrs.placement of
        AttachedToLocation lid -> do
          toDiscardBy iid (attrs.ability 1) attrs
          automaticallyEvadeEnemy iid enemyId
          discoverAt NotInvestigate iid (attrs.ability 1) lid 1
          nextPhaseModifier #upkeep (attrs.ability 1) enemyId DoesNotReadyDuringUpkeep
        _ -> error "impossible"
      pure e
    _ -> UnderSurveillance1 <$> liftRunMessage msg attrs
