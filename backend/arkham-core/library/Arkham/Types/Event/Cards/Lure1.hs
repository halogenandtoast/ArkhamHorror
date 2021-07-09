module Arkham.Types.Event.Cards.Lure1
  ( lure1
  , Lure1(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Helpers
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target
import Arkham.Types.Window

newtype Lure1 = Lure1 EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lure1 :: EventCard Lure1
lure1 = event Lure1 Cards.lure1

instance HasActions env Lure1 where
  getActions iid AtEndOfRound (Lure1 attrs)
    | isJust (eventAttachedTarget attrs) = pure
      [ UseAbility iid (mkAbility (toSource attrs) 1 ForcedAbility)
      | eventOwner attrs == iid
      ]
  getActions iid window (Lure1 attrs) = getActions iid window attrs

instance HasModifiersFor env Lure1 where
  getModifiersFor _ (EnemyTarget _) (Lure1 attrs) =
    case eventAttachedTarget attrs of
      Just target@(LocationTarget _) ->
        pure $ toModifiers attrs [DuringEnemyPhaseMustMoveToward target]
      Just _ -> pure []
      Nothing -> pure []
  getModifiersFor _ _ _ = pure []

instance HasId LocationId env InvestigatorId => RunMessage env Lure1 where
  runMessage msg e@(Lure1 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == toId attrs -> do
      lid <- getId iid
      e <$ push (AttachEvent eid (LocationTarget lid))
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      e <$ push (Discard (toTarget attrs))
    _ -> Lure1 <$> runMessage msg attrs
