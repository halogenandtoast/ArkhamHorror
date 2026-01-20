module Arkham.Location.Cards.TheKeyReliquaryLair (theKeyReliquaryLair) where

import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheKeyReliquaryLair = TheKeyReliquaryLair LocationAttrs
  deriving anyclass (IsLocation, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theKeyReliquaryLair :: LocationCard TheKeyReliquaryLair
theKeyReliquaryLair = location TheKeyReliquaryLair Cards.theKeyReliquaryLair 1 (PerPlayer 1)

instance HasModifiersFor TheKeyReliquaryLair where
  getModifiersFor (TheKeyReliquaryLair a) = do
    n <- selectCount UnstableScarletKey
    when (n > 0) $ modifySelf a [ShroudModifier (n * 2)]
