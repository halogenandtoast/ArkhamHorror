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
import Arkham.Placement
import Arkham.Projection
import Arkham.Target

newtype Barricade3 = Barricade3 EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
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

instance RunMessage Barricade3 where
  runMessage msg e@(Barricade3 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      lid <- fieldMap
        InvestigatorLocation
        (fromJustNote "must be at a location")
        iid
      e <$ push (PlaceEvent iid eid (AttachedToLocation lid))
    MoveFrom _ _ lid | LocationTarget lid `elem` eventAttachedTarget attrs ->
      e <$ push (Discard (EventTarget eventId))
    _ -> Barricade3 <$> runMessage msg attrs
