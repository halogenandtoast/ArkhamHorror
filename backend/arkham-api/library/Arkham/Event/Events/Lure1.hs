module Arkham.Event.Events.Lure1 (
  lure1,
  Lure1 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Helpers
import Arkham.Event.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Placement
import Arkham.Projection
import Arkham.Timing qualified as Timing

newtype Lure1 = Lure1 EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lure1 :: EventCard Lure1
lure1 = event Lure1 Cards.lure1

instance HasAbilities Lure1 where
  getAbilities (Lure1 attrs) =
    [restrictedAbility attrs 1 ControlsThis $ ForcedAbility $ RoundEnds Timing.When]

instance HasModifiersFor Lure1 where
  getModifiersFor (Lure1 attrs) =
    case eventAttachedTarget attrs of
      Just target@(LocationTarget _) ->
        modifySelect attrs AnyEnemy [DuringEnemyPhaseMustMoveToward target]
      Just _ -> pure mempty
      Nothing -> pure mempty

instance RunMessage Lure1 where
  runMessage msg e@(Lure1 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      lid <- fieldJust InvestigatorLocation iid
      push $ PlaceEvent eid $ AttachedToLocation lid
      pure e
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ toDiscardBy iid (toAbilitySource attrs 1) attrs
      pure e
    _ -> Lure1 <$> runMessage msg attrs
