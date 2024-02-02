module Arkham.Event.Cards.Lure2 (
  lure2,
  Lure2 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Helpers
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Placement
import Arkham.Timing qualified as Timing

newtype Lure2 = Lure2 EventAttrs
  deriving anyclass (IsEvent)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

lure2 :: EventCard Lure2
lure2 = event Lure2 Cards.lure2

instance HasAbilities Lure2 where
  getAbilities (Lure2 attrs) =
    [restrictedAbility attrs 1 ControlsThis $ ForcedAbility $ RoundEnds Timing.When]

instance HasModifiersFor Lure2 where
  getModifiersFor (EnemyTarget _) (Lure2 attrs) =
    case eventAttachedTarget attrs of
      Just target@(LocationTarget _) ->
        pure $ toModifiers attrs [DuringEnemyPhaseMustMoveToward target]
      Just _ -> pure []
      Nothing -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage Lure2 where
  runMessage msg e@(Lure2 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      lids <- selectList $ LocationMatchAny [locationWithInvestigator iid, ConnectedLocation]
      player <- getPlayer iid
      push
        $ chooseOne player [targetLabel lid [PlaceEvent iid eid $ AttachedToLocation lid] | lid <- lids]
      pure e
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ toDiscardBy iid (toAbilitySource attrs 1) attrs
      pure e
    _ -> Lure2 <$> runMessage msg attrs
