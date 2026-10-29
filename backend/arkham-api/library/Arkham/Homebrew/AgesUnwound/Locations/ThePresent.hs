module Arkham.Homebrew.AgesUnwound.Locations.ThePresent (thePresent) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (FewerActions))
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (revealedL)
import Arkham.Matcher

newtype ThePresent = ThePresent LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePresent :: LocationCard ThePresent
thePresent = symbolLabel $ locationWith ThePresent Cards.thePresent 3 (PerPlayer 2) $ revealedL .~ True

instance HasAbilities ThePresent where
  getAbilities (ThePresent a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ restricted a 1 (Here <> youExist YetToTakeTurn) (FastAbility Free)

instance RunMessage ThePresent where
  runMessage msg l@(ThePresent attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- Keep the spent action through the investigation phase's pool reset.
      roundModifier (attrs.ability 1) iid (FewerActions 1)
      takeActionAsIfTurnFromCurrentPool iid (attrs.ability 1)
      pure l
    _ -> ThePresent <$> liftRunMessage msg attrs
