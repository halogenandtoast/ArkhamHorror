module Arkham.Homebrew.AgesUnwound.Locations.RitualCircle_173 (ritualCircle_173) where

import Arkham.Ability
import Arkham.Action.Additional
import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.I18n
import Arkham.Location.Import.Lifted

newtype RitualCircle_173 = RitualCircle_173 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | /The Present, Fractured/. Scenario VI's own Ritual Circle, set aside until
-- act 3a puts every circle into play.
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
      {- TODO(ages-unwound): "which can only be used to investigate /Ritual
      Circle/" is narrowed to "to investigate".
      'Arkham.Action.Additional.AdditionalActionType' can restrict an extra
      action to an action type but not to a location, and an Investigate is not
      an ability, so 'AbilityMatchingAdditionalAction' cannot express it either.
      The haunted fires on this investigator's own turn, so the action is granted
      for the turn; spending it elsewhere costs a move they would have to pay for
      out of their standard actions, which this one is not. -}
      turnModifier iid (attrs.ability 1) iid
        $ GiveAdditionalAction
        $ AdditionalAction
          (ikey' "ritualCircleThePresentFractured.additionalAction")
          (attrs.ability 1)
          (ActionRestrictedAdditionalAction #investigate)
      pure l
    _ -> RitualCircle_173 <$> liftRunMessage msg attrs
