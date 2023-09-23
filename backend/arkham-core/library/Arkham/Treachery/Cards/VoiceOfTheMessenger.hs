module Arkham.Treachery.Cards.VoiceOfTheMessenger (
  voiceOfTheMessenger,
  VoiceOfTheMessenger (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype VoiceOfTheMessenger = VoiceOfTheMessenger TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

voiceOfTheMessenger :: TreacheryCard VoiceOfTheMessenger
voiceOfTheMessenger = treachery VoiceOfTheMessenger Cards.voiceOfTheMessenger

instance RunMessage VoiceOfTheMessenger where
  runMessage msg t@(VoiceOfTheMessenger attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      push
        $ chooseOne
          iid
          [ Label
              "Take 1 direct damage and suffer 1 physical trauma"
              [InvestigatorDirectDamage iid source 1 0, SufferTrauma iid 1 0]
          , Label
              "Take 1 direct horror and suffer 1 mental trauma"
              [InvestigatorDirectDamage iid source 0 1, SufferTrauma iid 0 1]
          ]
      pure t
    _ -> VoiceOfTheMessenger <$> runMessage msg attrs
