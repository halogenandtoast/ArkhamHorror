module Arkham.Asset.Assets.IcePick3 (icePick3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (getSkillTest, getSkillTestTargetedLocation, withSkillTest)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.SkillTest.Base (skillTestSubscribers)

newtype IcePick3 = IcePick3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

icePick3 :: AssetCard IcePick3
icePick3 = asset IcePick3 Cards.icePick3

instance HasAbilities IcePick3 where
  getAbilities (IcePick3 x) =
    [ controlled x 1 (DuringSkillTest $ YourSkillTest $ oneOf [#investigating, #fighting])
        $ FastAbility (exhaust x)
    ]

instance RunMessage IcePick3 where
  runMessage msg a@(IcePick3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) iid (AnySkillValue 1)
      push $ AddSubscriber (toTarget attrs)
      pure a
    PassedSkillTest iid (Just action) _ (Initiator _) _ _ | Just iid == attrs.controller -> do
      mTest <- getSkillTest
      let usedHere = maybe False ((toTarget attrs `elem`) . skillTestSubscribers) mTest
      when usedHere $ withSkillTest \sid -> do
        when (action == #fight) do
          chooseOneM iid do
            (cardI18n $ labeled' "icePick3.discardIcePick3ToDo1Damage") do
              toDiscardBy iid (attrs.ability 1) attrs
              skillTestModifier sid (attrs.ability 1) iid (DamageDealt 1)
            labeledI "doNotDiscardCard" nothing
        when (action == #investigate) do
          withLocationOf iid \loc -> do
            mTargetLoc <- getSkillTestTargetedLocation
            chooseOneM iid do
              (cardI18n $ labeled' "icePick3.discardIcePick3") do
                toDiscardBy iid (attrs.ability 1) attrs
                if mTargetLoc == Just loc
                  then skillTestModifier sid (attrs.ability 1) iid (DiscoveredClues 1)
                  else skillTestModifier sid (attrs.ability 1) iid (DiscoveredCluesAt loc 1)
              labeledI "doNotDiscardCard" nothing
      pure a
    _ -> IcePick3 <$> liftRunMessage msg attrs
