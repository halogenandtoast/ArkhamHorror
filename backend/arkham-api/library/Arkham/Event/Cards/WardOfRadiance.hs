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
    PlayThisEvent iid eid | eid == toId attrs -> do
      push $ RequestChaosTokens (toSource attrs) (Just iid) (Reveal 5) SetAside
      pure e
    RequestedChaosTokens (isSource attrs -> True) (Just iid) tokens -> do
      let
        msg' =
          if any ((`elem` [#bless, #eldersign]) . chaosTokenFace) tokens
            then Label "Cancel Treachery" [CancelNext (toSource attrs) RevelationMessage]
            else Label "No matching tokens" []
      player <- getPlayer iid
      pushAll [FocusChaosTokens tokens, chooseOne player [msg'], ResetChaosTokens (toSource attrs)]
      pure e
    _ -> WardOfRadiance <$> runMessage msg attrs
