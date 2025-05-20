module Arkham.Asset.Assets.Duke (duke) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Ability
import Arkham.Helpers.Investigator
import Arkham.Helpers.Location
import Arkham.Investigate
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Modifier
import Arkham.Window (defaultWindows)

newtype Duke = Duke AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

duke :: AssetCard Duke
duke = allyWith Duke Cards.duke (2, 3) noSlots

instance HasAbilities Duke where
  getAbilities (Duke a) =
    [ fightAbility a 1 (exhaust a) ControlsThis
    , delayAdditionalCostsWhen (youExist $ InvestigatorCanMoveTo (a.ability 2) Anywhere)
        $ investigateAbility a 2 (exhaust a) ControlsThis
    ]

instance RunMessage Duke where
  runMessage msg a@(Duke attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifiers sid (attrs.ability 1) iid [BaseSkillOf #combat 4, DamageDealt 1]
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let source = attrs.ability 2
      as <- select $ performableAbilityWithoutActionBy iid $ at_ (be iid) <> #basic <> #investigate
      let convert ab = noAOO $ decrease_ (ab {abilitySource = ProxySource ab.source source}) 1
      chooseOneM iid do
        for_ as \ab -> abilityLabeled iid (convert ab) nothing
        targetsM (getAccessibleLocations iid source) \lid' -> do
          moveTo attrs iid lid'
          doStep 1 msg
      pure a
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 2) -> do
      lid <- getJustLocation iid
      selectForMaybeM (BasicInvestigate lid) \ab ->
        whenM (getCanPerformAbility iid (defaultWindows iid) (decrease_ ab 1)) do
          sid <- getRandom
          skillTestModifiers sid (attrs.ability 2) iid [BaseSkillOf #intellect 4]
          investigate' <- mkInvestigateLocation sid iid (attrs.ability 2) lid
          push $ CheckAdditionalActionCosts iid (toTarget lid) #investigate [toMessage investigate']
      pure a
    UseThisAbility iid (ProxySource (LocationSource lid) (isAbilitySource attrs 2 -> True)) _ -> do
      selectForMaybeM (BasicInvestigate lid) \ab ->
        whenM (getCanPerformAbility iid (defaultWindows iid) $ decrease_ ab 1) do
          sid <- getRandom
          skillTestModifiers sid (attrs.ability 2) iid [BaseSkillOf #intellect 4]
          pushM $ mkInvestigateLocation sid iid (attrs.ability 2) lid
      pure a
    _ -> Duke <$> liftRunMessage msg attrs
