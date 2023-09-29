module Arkham.Event.Cards.WardOfProtection2 (
  wardOfProtection2,
  WardOfProtection2 (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner

newtype WardOfProtection2 = WardOfProtection2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wardOfProtection2 :: EventCard WardOfProtection2
wardOfProtection2 = event WardOfProtection2 Cards.wardOfProtection2

instance RunMessage WardOfProtection2 where
  runMessage msg e@(WardOfProtection2 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      pushAll
        [ CancelNext (toSource attrs) RevelationMessage
        , assignHorror iid eid 1
        ]

      pure e
    _ -> WardOfProtection2 <$> runMessage msg attrs
