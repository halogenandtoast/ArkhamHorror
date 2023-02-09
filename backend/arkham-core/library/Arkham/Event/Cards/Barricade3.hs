module Arkham.Event.Cards.Barricade3
  ( barricade3
  , Barricade3(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Helpers
import Arkham.Event.Runner
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Message
import Arkham.Ability
import Arkham.Matcher
import Arkham.Placement
import Arkham.Projection
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype Barricade3 = Barricade3 EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

barricade3 :: EventCard Barricade3
barricade3 = event Barricade3 Cards.barricade3

instance HasModifiersFor Barricade3 where
  getModifiersFor (LocationTarget lid) (Barricade3 attrs) =
    if LocationTarget lid `elem` eventAttachedTarget attrs
      then pure $ toModifiers
        attrs
        [CannotBeEnteredByNonElite, SpawnNonEliteAtConnectingInstead]
      else pure []
  getModifiersFor _ _ = pure []

instance HasAbilities Barricade3 where
  getAbilities (Barricade3 x) = case eventAttachedTarget x of
    Just (LocationTarget lid) ->
      [ mkAbility x 1 $ ForcedAbility $ Leaves Timing.When You $ LocationWithId
          lid
      ]
    _ -> []

instance RunMessage Barricade3 where
  runMessage msg e@(Barricade3 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      lid <- fieldMap
        InvestigatorLocation
        (fromJustNote "must be at a location")
        iid
      e <$ push (PlaceEvent iid eid (AttachedToLocation lid))
    UseCardAbility _ source 1 _ _ | isSource attrs source ->
      e <$ push (Discard (toAbilitySource attrs 1) $ toTarget attrs)
    _ -> Barricade3 <$> runMessage msg attrs
