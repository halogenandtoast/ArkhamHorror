module Arkham.Asset.Assets.DanielJameson (danielJameson) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Matcher

newtype DanielJameson = DanielJameson AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

danielJameson :: AssetCard DanielJameson
danielJameson = ally DanielJameson Cards.danielJameson (2, 2)

instance HasModifiersFor DanielJameson where
  getModifiersFor (DanielJameson a) = controllerGets a [SkillModifier #agility 1]

instance HasAbilities DanielJameson where
  getAbilities (DanielJameson a) =
    [ controlled_ a 1
        $ triggered (SkillTestResult #after You (oneOf [#fighting, #evading]) #failure) (exhaust a)
    ]

instance RunMessage DanielJameson where
  runMessage msg a@(DanielJameson attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      repeatSkillTestWith \sid -> modifyAnySkill sid (attrs.ability 1) iid 1
      pure a
    _ -> DanielJameson <$> liftRunMessage msg attrs
