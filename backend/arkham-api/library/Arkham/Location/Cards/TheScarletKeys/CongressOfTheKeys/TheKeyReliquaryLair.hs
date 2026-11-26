module Arkham.Location.Cards.TheScarletKeys.CongressOfTheKeys.TheKeyReliquaryLair (theKeyReliquaryLair) where

import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Location.CardDefs.TheScarletKeys.CongressOfTheKeys qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheKeyReliquaryLair = TheKeyReliquaryLair LocationAttrs
  deriving anyclass (IsLocation, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theKeyReliquaryLair :: LocationCard TheKeyReliquaryLair
theKeyReliquaryLair = location TheKeyReliquaryLair Cards.theKeyReliquaryLair 1 (PerPlayer 1)

instance HasModifiersFor TheKeyReliquaryLair where
  getModifiersFor (TheKeyReliquaryLair a) = do
    n <- selectCount UnstableScarletKey
    modifySelfWhen a (n > 0) [ShroudModifier (n * 2)]
