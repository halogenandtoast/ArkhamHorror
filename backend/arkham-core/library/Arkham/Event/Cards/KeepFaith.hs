module Arkham.Event.Cards.KeepFaith (keepFaith, KeepFaith (..)) where

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.ChaosBag
import Arkham.Prelude

newtype KeepFaith = KeepFaith EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

keepFaith :: EventCard KeepFaith
keepFaith = event KeepFaith Cards.keepFaith

instance RunMessage KeepFaith where
  runMessage msg e@(KeepFaith attrs) = case msg of
    PlayThisEvent _iid eid | eid == toId attrs -> do
      n <- min 4 <$> getRemainingCurseTokens
      pushAll $ replicate n $ AddChaosToken #bless
      pure e
    _ -> KeepFaith <$> runMessage msg attrs
