module Arkham.Event.Cards.Barricade3
  ( barricade3
  , Barricade3(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Event.Attrs
import Arkham.Event.Helpers
import Arkham.Event.Runner
import Arkham.Message
import Arkham.Modifier
import Arkham.Target

newtype Barricade3 = Barricade3 EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

barricade3 :: EventCard Barricade3
barricade3 = event Barricade3 Cards.barricade3

instance HasModifiersFor Barricade3 where
  getModifiersFor _ (LocationTarget lid) (Barricade3 attrs) =
    if LocationTarget lid `elem` eventAttachedTarget attrs
      then pure $ toModifiers
        attrs
        [CannotBeEnteredByNonElite, SpawnNonEliteAtConnectingInstead]
      else pure []
  getModifiersFor _ _ _ = pure []

instance (EventRunner env) => RunMessage Barricade3 where
  runMessage msg e@(Barricade3 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      lid <- getId iid
      e <$ push (AttachEvent eid (LocationTarget lid))
    MoveFrom _ _ lid | LocationTarget lid `elem` eventAttachedTarget ->
      e <$ push (Discard (EventTarget eventId))
    AttachEvent eid target | eid == eventId ->
      pure . Barricade3 $ attrs & attachedTargetL ?~ target
    _ -> Barricade3 <$> runMessage msg attrs
