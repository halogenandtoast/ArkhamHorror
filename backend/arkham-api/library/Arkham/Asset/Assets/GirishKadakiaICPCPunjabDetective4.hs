module Arkham.Asset.Assets.GirishKadakiaICPCPunjabDetective4 (
  girishKadakiaIcpcPunjabDetective4,
  GirishKadakiaICPCPunjabDetective4 (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.SkillTest (getSkillTestInvestigator, withSkillTest)
import Arkham.Matcher

newtype GirishKadakiaICPCPunjabDetective4 = GirishKadakiaICPCPunjabDetective4 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

girishKadakiaIcpcPunjabDetective4 :: AssetCard GirishKadakiaICPCPunjabDetective4
girishKadakiaIcpcPunjabDetective4 = ally GirishKadakiaICPCPunjabDetective4 Cards.girishKadakiaIcpcPunjabDetective4 (3, 3)

instance HasModifiersFor GirishKadakiaICPCPunjabDetective4 where
  getModifiersFor (GirishKadakiaICPCPunjabDetective4 a) = case a.controller of
    Nothing -> pure mempty
    Just iid ->
      modifySelect
        a
        (not_ (InvestigatorWithId iid) <> at_ (locationWithAsset a))
        [CanAssignDamageToAsset a.id, CanAssignHorrorToAsset a.id]

instance HasAbilities GirishKadakiaICPCPunjabDetective4 where
  getAbilities (GirishKadakiaICPCPunjabDetective4 a) =
    let criteria = DuringSkillTest $ SkillTestOfInvestigator $ affectsOthers $ InvestigatorAt YourLocation
     in [controlledAbility a 1 criteria $ FastAbility (exhaust a)]

instance RunMessage GirishKadakiaICPCPunjabDetective4 where
  runMessage msg a@(GirishKadakiaICPCPunjabDetective4 attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> do
        getSkillTestInvestigator >>= traverse_ \iid -> do
          skillTestModifier sid (attrs.ability 1) iid (AnySkillValue 2)
        push $ AddSubscriber (toTarget attrs)
      pure a
    PassedSkillTest _ _ _ (isTarget attrs -> True) _ _ -> do
      canHealHorror <- attrs.id <=~> HealableAsset (attrs.ability 1) #horror AnyAsset
      canHealDamage <- attrs.id <=~> HealableAsset (attrs.ability 1) #damage AnyAsset
      when (canHealHorror || canHealDamage) $ do
        for_ attrs.controller \iid ->
          chooseOne iid
            $ [healAssetDamage attrs (attrs.ability 1) 1 | canHealDamage]
            <> [healAssetHorror attrs (attrs.ability 1) 1 | canHealHorror]
      pure a
    _ -> GirishKadakiaICPCPunjabDetective4 <$> liftRunMessage msg attrs
