module Arkham.Types.Event.Cards.Barricade (barricade, Barricade(..)) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Helpers
import Arkham.Types.Event.Runner
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target

newtype Barricade = Barricade EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

barricade :: EventCard Barricade
barricade = event Barricade Cards.barricade

instance HasModifiersFor env Barricade where
  getModifiersFor _ (LocationTarget lid) (Barricade attrs) = pure $ toModifiers
    attrs
    [ CannotBeEnteredByNonElite
    | LocationTarget lid `elem` eventAttachedTarget attrs
    ]
  getModifiersFor _ _ _ = pure []

instance HasActions env Barricade where
  getActions i window (Barricade attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env Barricade where
  runMessage msg e@(Barricade attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      lid <- getId iid
      e <$ unshiftMessage (AttachEvent eid (LocationTarget lid))
    MoveFrom _ lid | LocationTarget lid `elem` eventAttachedTarget ->
      e <$ unshiftMessage (Discard (EventTarget eventId))
    _ -> Barricade <$> runMessage msg attrs
