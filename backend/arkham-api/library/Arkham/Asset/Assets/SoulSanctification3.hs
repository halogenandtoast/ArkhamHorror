module Arkham.Asset.Assets.SoulSanctification3 (soulSanctification3, SoulSanctification3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Game.Helpers (sourceMatches)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Modifier

newtype SoulSanctification3 = SoulSanctification3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

soulSanctification3 :: AssetCard SoulSanctification3
soulSanctification3 = asset SoulSanctification3 Cards.soulSanctification3

instance HasModifiersFor SoulSanctification3 where
  getModifiersFor (InvestigatorTarget _) (SoulSanctification3 a) = case a.controller of
    Nothing -> pure []
    Just controller -> modified a $ CanHealAtFull (sourceOwnedBy controller) <$> [#damage, #horror]
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
    ExcessHealDamage _iid source n -> do
      controlled <- maybe (pure False) (sourceMatches source . sourceOwnedBy) attrs.controller
      if controlled
        then liftRunMessage (PlaceTokens (toSource attrs) (toTarget attrs) Offering n) a
        else pure a
    ExcessHealHorror _iid source n -> do
      controlled <- maybe (pure False) (sourceMatches source . sourceOwnedBy) attrs.controller
      if controlled
        then liftRunMessage (PlaceTokens (toSource attrs) (toTarget attrs) Offering n) a
        else pure a
    _ -> SoulSanctification3 <$> liftRunMessage msg attrs
