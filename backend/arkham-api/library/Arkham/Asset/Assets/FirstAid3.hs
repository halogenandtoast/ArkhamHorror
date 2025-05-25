module Arkham.Asset.Assets.FirstAid3 (firstAid3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Damage
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype FirstAid3 = FirstAid3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

firstAid3 :: AssetCard FirstAid3
firstAid3 = assetWith FirstAid3 Cards.firstAid3 (whenNoUsesL ?~ DiscardWhenNoUses)

healableAsset :: Sourceable source => source -> DamageType -> LocationMatcher -> AssetMatcher
healableAsset (toSource -> source) hType loc = HealableAsset source hType $ at_ loc <> AssetControlledBy (affectsOthers Anyone)

instance HasAbilities FirstAid3 where
  getAbilities (FirstAid3 x) = [controlled x 1 criteria $ actionAbilityWithCost (assetUseCost x Supply 1)]
   where
    healable hType = HealableInvestigator (toSource x) hType $ at_ YourLocation
    criteria =
      oneOf
        [ exists $ oneOf $ map healable [#horror, #damage]
        , exists $ oneOf $ map (\hType -> healableAsset x hType YourLocation) [#damage, #horror]
        ]

instance RunMessage FirstAid3 where
  runMessage msg a@(FirstAid3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- [ALERT] [SAME] MedicalStudent
      let source = attrs.ability 1
      chooseOneM iid do
        horrorInvestigators <- select $ HealableInvestigator source #horror $ colocatedWith iid
        damageInvestigators <- select $ HealableInvestigator source #damage $ colocatedWith iid
        targets (nub $ horrorInvestigators <> damageInvestigators) \i -> do
          chooseOneAtATimeM iid do
            when (i `elem` damageInvestigators) $ damageLabeled iid $ healDamage iid source 1
            when (i `elem` horrorInvestigators) $ horrorLabeled iid $ healHorror iid source 1

        horrorAssets <- select $ healableAsset source #horror (locationWithInvestigator iid)
        damageAssets <- select $ healableAsset source #damage (locationWithInvestigator iid)
        targets (nub $ horrorAssets <> damageAssets) \asset' -> do
          chooseOneAtATimeM iid do
            when (asset' `elem` damageAssets) $ assetDamageLabeled asset' $ healDamage asset' source 1
            when (asset' `elem` horrorAssets) $ assetHorrorLabeled asset' $ healHorror asset' source 1
      pure a
    _ -> FirstAid3 <$> liftRunMessage msg attrs
