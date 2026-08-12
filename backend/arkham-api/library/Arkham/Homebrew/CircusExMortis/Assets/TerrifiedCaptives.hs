module Arkham.Homebrew.CircusExMortis.Assets.TerrifiedCaptives (terrifiedCaptives) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Trait (Trait (Tool))

newtype TerrifiedCaptives = TerrifiedCaptives AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

terrifiedCaptives :: AssetCard TerrifiedCaptives
terrifiedCaptives = asset TerrifiedCaptives Cards.terrifiedCaptives

instance HasModifiersFor TerrifiedCaptives where
  getModifiersFor (TerrifiedCaptives a) = case a.placement of
    AttachedToLocation _ -> modifySelect a AnyAgenda [DoomThresholdModifier (-1)]
    _ -> pure mempty

instance HasAbilities TerrifiedCaptives where
  getAbilities (TerrifiedCaptives a) =
    [skillTestAbility $ restricted a 1 OnSameLocation actionAbility]

instance RunMessage TerrifiedCaptives where
  runMessage msg a@(TerrifiedCaptives attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      unrevealed <- select $ UnrevealedLocation <> LocationWithoutInvestigators
      candidates <-
        if null unrevealed then select LocationWithoutInvestigators else pure unrevealed
      chooseOrRunOneM iid $ targets candidates $ place attrs . AttachedToLocation
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      hasTool <- selectAny $ assetControlledBy iid <> AssetWithTrait Tool
      chooseOneM iid do
        for_ [#combat, #agility] \sType ->
          skillLabeled sType
            $ beginSkillTest sid iid (attrs.ability 1) attrs sType (Fixed $ if hasTool then 3 else 4)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      addToVictory iid attrs
      pure a
    _ -> TerrifiedCaptives <$> liftRunMessage msg attrs
