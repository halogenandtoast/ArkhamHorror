module Arkham.Event.Cards.VoiceOfRa
  ( voiceOfRa
  , VoiceOfRa(..)
  ) where

import Arkham.Prelude

import Arkham.ChaosBag.RevealStrategy
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Message
import Arkham.RequestedChaosTokenStrategy
import Arkham.ChaosToken

newtype VoiceOfRa = VoiceOfRa EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

voiceOfRa :: EventCard VoiceOfRa
voiceOfRa = event VoiceOfRa Cards.voiceOfRa

instance RunMessage VoiceOfRa where
  runMessage msg e@(VoiceOfRa attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      push $ RequestChaosTokens (toSource attrs) (Just iid) (Reveal 1) SetAside
      pure e
    RequestedChaosTokens (isSource attrs -> True) (Just iid) (map chaosTokenFace -> tokens)
      -> do
        push $ ResetChaosTokens (toSource attrs)
        let
          n =
            count (`elem` [Skull, Cultist, Tablet, ElderThing, AutoFail]) tokens
        push $ TakeResources iid (1 + (2 * n)) (toSource attrs) False
        pure e
    _ -> VoiceOfRa <$> runMessage msg attrs
