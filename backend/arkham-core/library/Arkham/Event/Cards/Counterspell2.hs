module Arkham.Event.Cards.Counterspell2 (
  counterspell2,
  Counterspell2 (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Game.Helpers
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype Counterspell2 = Counterspell2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

counterspell2 :: EventCard Counterspell2
counterspell2 = event Counterspell2 Cards.counterspell2

instance RunMessage Counterspell2 where
  runMessage msg e@(Counterspell2 attrs) = case msg of
    InvestigatorPlayEvent _ eid _ [(windowType -> Window.RevealChaosToken _ token)] _ | eid == toId attrs -> do
      cancelChaosToken token
      pushAll
        [ CancelEachNext (toSource attrs) [RunWindowMessage, DrawChaosTokenMessage, RevealChaosTokenMessage]
        ]
      pure e
    _ -> Counterspell2 <$> runMessage msg attrs
