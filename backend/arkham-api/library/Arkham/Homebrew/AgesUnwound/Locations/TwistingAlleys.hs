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

instance HasAbilities TwistingAlleys where
  getAbilities (TwistingAlleys a) =
    extendRevealed1 a
      $ skillTestAbility
      $ restricted a 1 (MovingTo RevealedLocation)
      $ forced
      $ Leaves #when You (be a)

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
