module Arkham.Event.Events.VoiceOfRa (voiceOfRa) where

import Arkham.Card
import Arkham.ChaosToken
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Taboo

newtype VoiceOfRa = VoiceOfRa EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

voiceOfRa :: EventCard VoiceOfRa
voiceOfRa = event VoiceOfRa Cards.voiceOfRa

instance RunMessage VoiceOfRa where
  runMessage msg e@(VoiceOfRa attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      requestChaosTokens iid attrs 3
      pure e
    RequestedChaosTokens (isSource attrs -> True) (Just iid) (map chaosTokenFace -> tokens) -> do
      send $ format (toCard attrs) <> " drew " <> toSentence (map chaosTokenLabel tokens)
      continue_ iid
      let valid =
            if tabooed TabooList20 attrs
              then isSymbolChaosToken
              else (`elem` [Skull, Cultist, Tablet, ElderThing, AutoFail])
      gainResources iid attrs (1 + (2 * count valid tokens))
      pure e
    _ -> VoiceOfRa <$> liftRunMessage msg attrs
