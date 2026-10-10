module Arkham.Homebrew.AgesUnwound.Locations.TwistingAlleys (twistingAlleys) where

import Arkham.Ability
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.NightOfFire.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype TwistingAlleys = TwistingAlleys LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

twistingAlleys :: LocationCard TwistingAlleys
twistingAlleys = symbolLabel $ location TwistingAlleys Cards.twistingAlleys 4 (PerPlayer 1)

{- | "Forced - When you would leave Twisting Alleys, if you are not moving to a
new Arkham Streets location: Test [willpower] or [agility] (3). If you fail, move
to a new Arkham Streets location instead of your original destination. If no such
locations exist, instead cancel the effects of the move."

"Not moving to a new Arkham Streets location" is read as "the destination is
already revealed": a new one is always put into play facedown and is revealed
the moment you arrive, so an unrevealed destination can only be a street that
was just dealt (or the one Eternity's Sentinel was placed on).
-}
instance HasAbilities TwistingAlleys where
  getAbilities (TwistingAlleys a) =
    extendRevealed1 a
      $ skillTestAbility
      $ forcedAbility a 1
      $ WouldMove #when You #any (be a) RevealedLocation

instance RunMessage TwistingAlleys where
  runMessage msg l@(TwistingAlleys attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid (attrs.ability 1) iid [#willpower, #agility] (Fixed 3)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      redirectToNewArkhamStreetsLocation (attrs.ability 1) iid
      pure l
    _ -> TwistingAlleys <$> liftRunMessage msg attrs
