module Arkham.Asset.Cards.SoulSanctification3 (soulSanctification3, SoulSanctification3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Modifier

newtype SoulSanctification3 = SoulSanctification3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

soulSanctification3 :: AssetCard SoulSanctification3
soulSanctification3 = asset SoulSanctification3 Cards.soulSanctification3

instance HasModifiersFor SoulSanctification3 where
  getModifiersFor (InvestigatorTarget iid) (SoulSanctification3 a) | a `controlledBy` iid = do
    modified a [CanHealAtFull #damage, CanHealAtFull #horror]
  getModifiersFor _ _ = pure []

instance HasAbilities SoulSanctification3 where
  getAbilities (SoulSanctification3 a) =
    [ limitedAbility (PlayerLimit PerTestOrAbility 2)
        $ controlledAbility a 1 DuringAnySkillTest
        $ FastAbility (assetUseCost a Offering 1)
    ]

instance RunMessage SoulSanctification3 where
  runMessage msg a@(SoulSanctification3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) iid (AnySkillValue 2)
      pure a
    ExcessHealDamage iid n | attrs `controlledBy` iid -> do
      runMessage (PlaceTokens (toSource attrs) (toTarget attrs) Offering n) a
    ExcessHealHorror iid n | attrs `controlledBy` iid -> do
      runMessage (PlaceTokens (toSource attrs) (toTarget attrs) Offering n) a
    _ -> SoulSanctification3 <$> liftRunMessage msg attrs
