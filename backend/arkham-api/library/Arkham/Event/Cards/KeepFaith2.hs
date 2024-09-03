module Arkham.Event.Cards.KeepFaith2 (keepFaith2, KeepFaith2 (..)) where

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.ChaosBag
import Arkham.Prelude

newtype KeepFaith2 = KeepFaith2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

keepFaith2 :: EventCard KeepFaith2
keepFaith2 = event KeepFaith2 Cards.keepFaith2

instance RunMessage KeepFaith2 where
  runMessage msg e@(KeepFaith2 attrs) = case msg of
    PlayThisEvent _iid eid | eid == toId attrs -> do
      n <- min 4 <$> getRemainingCurseTokens
      pushAll $ replicate n $ AddChaosToken #bless
      pure e
    _ -> KeepFaith2 <$> runMessage msg attrs
