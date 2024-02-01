module Arkham.Event.Cards.Barricade3 (
  barricade3,
  Barricade3 (..),
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

newtype Barricade3 = Barricade3 EventAttrs
  deriving anyclass (IsEvent)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

barricade3 :: EventCard Barricade3
barricade3 = event Barricade3 Cards.barricade3

instance HasModifiersFor Barricade3 where
  getModifiersFor (LocationTarget lid) (Barricade3 attrs) =
    pure
      $ toModifiers attrs
      $ guard (LocationTarget lid `elem` eventAttachedTarget attrs)
      *> [CannotBeEnteredBy NonEliteEnemy, SpawnNonEliteAtConnectingInstead]
  getModifiersFor _ _ = pure []

instance HasAbilities Barricade3 where
  getAbilities (Barricade3 x) = case eventAttachedTarget x of
    Just (LocationTarget lid) ->
      [ mkAbility x 1
          $ ForcedAbility
          $ Leaves #when You
          $ LocationWithId lid
      ]
    _ -> []

instance RunMessage Barricade3 where
  runMessage msg e@(Barricade3 attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      lid <- fieldJust InvestigatorLocation iid
      push $ PlaceEvent iid eid (AttachedToLocation lid)
      pure e
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ toDiscardBy iid (toAbilitySource attrs 1) attrs
      pure e
    _ -> Barricade3 <$> runMessage msg attrs
