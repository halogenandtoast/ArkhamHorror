module Arkham.Event.Cards.WardOfProtection5 (
  wardOfProtection5,
  WardOfProtection5 (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner

newtype WardOfProtection5 = WardOfProtection5 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

wardOfProtection5 :: EventCard WardOfProtection5
wardOfProtection5 = event WardOfProtection5 Cards.wardOfProtection5

instance RunMessage WardOfProtection5 where
  runMessage msg e@(WardOfProtection5 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      pushAll
        [ CancelNext (toSource attrs) RevelationMessage
        , CancelNext (toSource attrs) DrawEnemyMessage
        , CancelSurge (toSource attrs)
        , assignHorror iid eid 1
        ]
      pure e
    _ -> WardOfProtection5 <$> runMessage msg attrs
