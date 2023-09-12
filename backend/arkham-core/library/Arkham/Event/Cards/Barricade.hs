module Arkham.Event.Cards.Barricade (
  barricade,
  Barricade (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Helpers
import Arkham.Event.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message
import Arkham.Placement
import Arkham.Projection
import Arkham.Timing qualified as Timing

newtype Barricade = Barricade EventAttrs
  deriving anyclass (IsEvent)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

barricade :: EventCard Barricade
barricade = event Barricade Cards.barricade

instance HasModifiersFor Barricade where
  getModifiersFor target (Barricade attrs) | target `elem` eventAttachedTarget attrs = do
    pure $ toModifiers attrs [CannotBeEnteredByNonElite]
  getModifiersFor _ _ = pure []

instance HasAbilities Barricade where
  getAbilities (Barricade x) = case eventAttachedTarget x of
    Just (LocationTarget lid) -> [forcedAbility x 1 $ Leaves Timing.When You $ LocationWithId lid]
    _ -> []

instance RunMessage Barricade where
  runMessage msg e@(Barricade attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      lid <- fieldJust InvestigatorLocation iid
      push $ PlaceEvent iid eid (AttachedToLocation lid)
      pure e
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ Discard (toAbilitySource attrs 1) (toTarget attrs)
      pure e
    _ -> Barricade <$> runMessage msg attrs
