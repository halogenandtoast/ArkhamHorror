module Arkham.Event.Cards.WardOfRadiance (
  wardOfRadiance,
  WardOfRadiance (..),
)
where

import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Prelude
import Arkham.RequestedChaosTokenStrategy

newtype WardOfRadiance = WardOfRadiance EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wardOfRadiance :: EventCard WardOfRadiance
wardOfRadiance = event WardOfRadiance Cards.wardOfRadiance

instance RunMessage WardOfRadiance where
  runMessage msg e@(WardOfRadiance attrs) = case msg of
    PlayThisEvent _iid eid | eid == toId attrs -> do
      push $ RequestChaosTokens (toSource attrs) Nothing (Reveal 5) SetAside
      pure e
    RequestedChaosTokens (isSource attrs -> True) _ tokens -> do
      pushWhen (any ((`elem` [#bless, #eldersign]) . chaosTokenFace) tokens)
        $ CancelNext (toSource attrs) RevelationMessage
      pure e
    _ -> WardOfRadiance <$> runMessage msg attrs
