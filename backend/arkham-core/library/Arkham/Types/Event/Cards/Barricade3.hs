module Arkham.Types.Event.Cards.Barricade3 (barricade3, Barricade3(..)) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Helpers
import Arkham.Types.Event.Runner
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target

newtype Barricade3 = Barricade3 EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

barricade3 :: EventCard Barricade3
barricade3 = event Barricade3 Cards.barricade3

instance HasModifiersFor env Barricade3 where
  getModifiersFor _ (LocationTarget lid) (Barricade3 attrs) =
    if LocationTarget lid `elem` eventAttachedTarget attrs
      then pure $ toModifiers
        attrs
        [CannotBeEnteredByNonElite, SpawnNonEliteAtConnectingInstead]
      else pure []
  getModifiersFor _ _ _ = pure []

instance HasActions env Barricade3 where
  getActions i window (Barricade3 attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env Barricade3 where
  runMessage msg e@(Barricade3 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      lid <- getId iid
      e <$ unshiftMessage (AttachEvent eid (LocationTarget lid))
    MoveFrom _ lid | LocationTarget lid `elem` eventAttachedTarget ->
      e <$ unshiftMessage (Discard (EventTarget eventId))
    AttachEvent eid target | eid == eventId ->
      pure . Barricade3 $ attrs & attachedTargetL ?~ target
    _ -> Barricade3 <$> runMessage msg attrs
