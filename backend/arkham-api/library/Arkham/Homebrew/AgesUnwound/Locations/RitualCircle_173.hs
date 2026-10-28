module Arkham.Homebrew.AgesUnwound.Locations.RitualCircle_173 (ritualCircle_173) where

import Arkham.Ability
import Arkham.Action.Additional
import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.I18n
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype RitualCircle_173 = RitualCircle_173 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ritualCircle_173 :: LocationCard RitualCircle_173
ritualCircle_173 = symbolLabel $ location RitualCircle_173 Cards.ritualCircle_173 4 (PerPlayer 2)

instance HasModifiersFor RitualCircle_173 where
  getModifiersFor (RitualCircle_173 a) = modifySelf a [CountsAsDifferentLocation]

instance HasAbilities RitualCircle_173 where
  getAbilities (RitualCircle_173 a) =
    extendRevealed1 a $ campaignI18n (hauntedI "ritualCircleThePresentFractured.haunted" a 1)

instance RunMessage RitualCircle_173 where
  runMessage msg l@(RitualCircle_173 attrs) = runQueueT $ campaignI18n $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamageAndHorror iid (attrs.ability 1) 1 1
      placeCluesOnLocation iid (attrs.ability 1) 1
      turnModifier iid (attrs.ability 1) iid
        $ GiveAdditionalAction
        $ AdditionalAction
          (ikey' "ritualCircleThePresentFractured.additionalAction")
          (attrs.ability 1)
          (AbilityMatchingAdditionalAction $ AbilityOnLocation (be attrs) <> AbilityIsAction #investigate)
      pure l
    _ -> RitualCircle_173 <$> liftRunMessage msg attrs
