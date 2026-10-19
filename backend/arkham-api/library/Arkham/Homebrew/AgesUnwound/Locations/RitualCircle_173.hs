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

{- | /The Present, Fractured/. Scenario VI's own Ritual Circle, set aside until
act 3a puts every circle into play.
-}
ritualCircle_173 :: LocationCard RitualCircle_173
ritualCircle_173 = symbolLabel $ location RitualCircle_173 Cards.ritualCircle_173 4 (PerPlayer 2)

{- | "Each investigator at Ritual Circle is considered to be at a different copy
of Ritual Circle."

'CountsAsDifferentLocation' is the engine's own word for it -- the modifier
Return to Dim Carcosa's /Recesses of Your Own Mind/ uses for the same printed
clause.
-}
instance HasModifiersFor RitualCircle_173 where
  getModifiersFor (RitualCircle_173 a) = modifySelf a [CountsAsDifferentLocation]

{- | "Haunted - Take 1 damage and 1 horror. Place 1 of your clues on Ritual
Circle. Gain an action, which can only be used to investigate Ritual Circle."
-}
instance HasAbilities RitualCircle_173 where
  getAbilities (RitualCircle_173 a) =
    extendRevealed1 a $ campaignI18n (hauntedI "ritualCircleThePresentFractured.haunted" a 1)

instance RunMessage RitualCircle_173 where
  runMessage msg l@(RitualCircle_173 attrs) = runQueueT $ campaignI18n $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamageAndHorror iid (attrs.ability 1) 1 1
      -- The haunted resolves at the location the failed test happened at, which
      -- is this one, so "your location" and "Ritual Circle" are the same place.
      placeCluesOnLocation iid (attrs.ability 1) 1
      {- "which can only be used to investigate /Ritual Circle/" -- both halves,
      not just the action type. A location's basic Investigate is an ability it
      publishes from its own attrs (@Location.Runner@'s 'HasAbilities'), so the
      pair is an 'AbilityMatcher' and 'AbilityMatchingAdditionalAction' carries
      it. 'ActionRestrictedAdditionalAction' could only have said "to
      investigate", leaving the action spendable at any location. -}
      turnModifier iid (attrs.ability 1) iid
        $ GiveAdditionalAction
        $ AdditionalAction
          (ikey' "ritualCircleThePresentFractured.additionalAction")
          (attrs.ability 1)
          (AbilityMatchingAdditionalAction $ AbilityOnLocation (be attrs) <> AbilityIsAction #investigate)
      pure l
    _ -> RitualCircle_173 <$> liftRunMessage msg attrs
