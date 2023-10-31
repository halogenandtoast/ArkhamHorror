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
import Arkham.Placement
import Arkham.Projection

newtype Barricade = Barricade EventAttrs
  deriving anyclass (IsEvent)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

barricade :: EventCard Barricade
barricade = event Barricade Cards.barricade

instance HasModifiersFor Barricade where
  getModifiersFor target (Barricade attrs) | target `elem` eventAttachedTarget attrs = do
    pure $ toModifiers attrs [CannotBeEnteredBy NonEliteEnemy]
  getModifiersFor _ _ = pure []

instance HasAbilities Barricade where
  getAbilities (Barricade x) = case eventAttachedTarget x of
    Just (LocationTarget lid) -> [forcedAbility x 1 $ Leaves #when You $ LocationWithId lid]
    _ -> []

instance RunMessage Barricade where
  runMessage msg e@(Barricade attrs) = case msg of
    PlayThisEvent iid eid | attrs `is` eid -> do
      lid <- fieldJust InvestigatorLocation iid
      push $ PlaceEvent iid eid (AttachedToLocation lid)
      pure e
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ toDiscardBy iid (toAbilitySource attrs 1) attrs
      pure e
    _ -> Barricade <$> runMessage msg attrs
