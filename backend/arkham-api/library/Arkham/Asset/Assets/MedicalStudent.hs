module Arkham.Asset.Assets.MedicalStudent (medicalStudent) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Damage
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype MedicalStudent = MedicalStudent AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

medicalStudent :: AssetCard MedicalStudent
medicalStudent = ally MedicalStudent Cards.medicalStudent (1, 1)

healableAsset :: Sourceable source => source -> DamageType -> LocationMatcher -> AssetMatcher
healableAsset (toSource -> source) hType loc = HealableAsset source hType $ at_ loc <> AssetControlledBy (affectsOthers Anyone)

instance HasAbilities MedicalStudent where
  getAbilities (MedicalStudent x) =
    [controlled x 1 criteria $ freeReaction (AssetEntersPlay #when (be x))]
   where
    healable hType = HealableInvestigator (toSource x) hType $ at_ YourLocation
    criteria =
      oneOf
        [ exists $ oneOf $ map healable [#horror, #damage]
        , exists $ oneOf [healableAsset x kind YourLocation | kind <- [#damage, #horror]]
        ]

instance RunMessage MedicalStudent where
  runMessage msg a@(MedicalStudent attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- [ALERT] [SAME] FirstAid3
      let source = attrs.ability 1
      chooseOneM iid do
        horrorInvestigators <- select $ HealableInvestigator source #horror $ colocatedWith iid
        damageInvestigators <- select $ HealableInvestigator source #damage $ colocatedWith iid
        targets (nub $ horrorInvestigators <> damageInvestigators) \i -> do
          chooseOneAtATimeM iid do
            when (i `elem` damageInvestigators) $ damageLabeled i $ healDamage i source 1
            when (i `elem` horrorInvestigators) $ horrorLabeled i $ healHorror i source 1

        horrorAssets <- select $ healableAsset source #horror (locationWithInvestigator iid)
        damageAssets <- select $ healableAsset source #damage (locationWithInvestigator iid)
        targets (nub $ horrorAssets <> damageAssets) \asset' -> do
          chooseOneAtATimeM iid do
            when (asset' `elem` damageAssets) $ assetDamageLabeled asset' $ healDamage asset' source 1
            when (asset' `elem` horrorAssets) $ assetHorrorLabeled asset' $ healHorror asset' source 1
      pure a
    _ -> MedicalStudent <$> liftRunMessage msg attrs
