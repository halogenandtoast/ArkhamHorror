module Arkham.Event.Events.WardOfRadiance (wardOfRadiance, WardOfRadiance (..)) where

import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Window (cardDrawn)
import Arkham.RequestedChaosTokenStrategy

newtype WardOfRadiance = WardOfRadiance EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wardOfRadiance :: EventCard WardOfRadiance
wardOfRadiance = event WardOfRadiance Cards.wardOfRadiance

instance RunMessage WardOfRadiance where
  runMessage msg e@(WardOfRadiance attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      push $ RequestChaosTokens (toSource attrs) (Just iid) (Reveal 5) SetAside
      pure e
    RequestedChaosTokens (isSource attrs -> True) (Just iid) tokens -> do
      chooseOneM iid do
        if any ((`elem` [#bless, #eldersign]) . chaosTokenFace) tokens
          then labeled "Cancel Revelation Effect" $ cancelRevelation attrs (cardDrawn attrs.windows)
          else labeled "No matching tokens" nothing

      push $ ResetChaosTokens (toSource attrs)
      pure e
    _ -> WardOfRadiance <$> liftRunMessage msg attrs
