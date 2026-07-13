module Arkham.Asset.Assets.LocalMap (localMap) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Cost
import Arkham.Helpers.Location (getCanMoveTo)
import Arkham.Helpers.SkillTest.Lifted (getSkillTestTargetedLocation)
import Arkham.I18n
import Arkham.Investigate (mkInvestigateLocation)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Modifier

newtype LocalMap = LocalMap AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

localMap :: AssetCard LocalMap
localMap = asset LocalMap Cards.localMap

instance HasAbilities LocalMap where
  getAbilities (LocalMap a) =
    [ skillTestAbility
        $ withInvestigationTargets (#revealed <> connectedFrom YourLocation)
        $ controlled_ a 1 (investigateAction $ assetUseCost a Secret 1)
    ]

instance RunMessage LocalMap where
  runMessage msg a@(LocalMap attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- select $ #revealed <> connectedFrom (locationWithInvestigator iid) <> #investigatable
      locationsWithCosts <-
        filterM
          (\lid -> getCanAffordAdditionalActionCost iid attrs (toTarget lid) #investigate)
          locations
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid (SkillModifier #intellect 1)
      chooseOrRunOneM iid $ targets locationsWithCosts \lid -> do
        investigate' <- mkInvestigateLocation sid iid (attrs.ability 1) lid
        push $ CheckAdditionalActionCosts iid (toTarget lid) #investigate [toMessage investigate']
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) | attrs.ready -> do
      whenJustM getSkillTestTargetedLocation \loc -> do
        whenM (getCanMoveTo iid (attrs.ability 1) loc) do
          chooseOneM iid $ cardI18n $ scope "localMap" do
            labeled' "exhaustAndMove" do
              exhaustThis attrs
              moveTo (attrs.ability 1) iid loc
            withI18n skip_
      pure a
    _ -> LocalMap <$> liftRunMessage msg attrs
