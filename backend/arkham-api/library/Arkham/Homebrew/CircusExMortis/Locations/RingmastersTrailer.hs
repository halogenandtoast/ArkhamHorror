module Arkham.Homebrew.CircusExMortis.Locations.RingmastersTrailer (ringmastersTrailer) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Choose

newtype RingmastersTrailer = RingmastersTrailer LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ringmastersTrailer :: LocationCard RingmastersTrailer
ringmastersTrailer = location RingmastersTrailer Cards.ringmastersTrailer 3 (Static 1)

instance HasAbilities RingmastersTrailer where
  getAbilities (RingmastersTrailer a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ skillTestAbility
      $ restricted a 1 Here actionAbility

instance RunMessage RingmastersTrailer where
  runMessage msg l@(RingmastersTrailer attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid (attrs.ability 1) iid [#willpower, #intellect] (Fixed 3)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      drawCards iid (attrs.ability 1) 2
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignHorror iid (attrs.ability 1) 1
      pure l
    _ -> RingmastersTrailer <$> liftRunMessage msg attrs
