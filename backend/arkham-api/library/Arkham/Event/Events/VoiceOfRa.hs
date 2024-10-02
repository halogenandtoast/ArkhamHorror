module Arkham.Event.Events.VoiceOfRa (voiceOfRa, VoiceOfRa (..)) where

import Arkham.Card
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.RequestedChaosTokenStrategy
import Arkham.Taboo

newtype VoiceOfRa = VoiceOfRa EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

voiceOfRa :: EventCard VoiceOfRa
voiceOfRa = event VoiceOfRa Cards.voiceOfRa

instance RunMessage VoiceOfRa where
  runMessage msg e@(VoiceOfRa attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      push $ RequestChaosTokens (toSource attrs) (Just iid) (Reveal 3) SetAside
      pure e
    RequestedChaosTokens (isSource attrs -> True) (Just iid) (map chaosTokenFace -> tokens) -> do
      send $ format (toCard attrs) <> " drew " <> toSentence (map chaosTokenLabel tokens)
      continue iid []
      let valid =
            if tabooed TabooList20 attrs
              then isSymbolChaosToken
              else (`elem` [Skull, Cultist, Tablet, ElderThing, AutoFail])
      let n = count valid tokens
      gainResourcesIfCan iid attrs (1 + (2 * n))
      push $ ResetChaosTokens (toSource attrs)
      pure e
    _ -> VoiceOfRa <$> liftRunMessage msg attrs
