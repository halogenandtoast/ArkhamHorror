module Arkham.Asset.Assets.KeyLocusLastBastion (keyLocusLastBastion) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (AssetDefeated)
import Arkham.GameValue
import Arkham.Helpers.Modifiers (modifySelf)
import Arkham.Matcher
import Arkham.Scenarios.DogsOfWar.Helpers (pattern IsKeyLocus)

newtype KeyLocusLastBastion = KeyLocusLastBastion AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

keyLocusLastBastion :: AssetCard KeyLocusLastBastion
keyLocusLastBastion = assetWith KeyLocusLastBastion Cards.keyLocusLastBastion (healthL ?~ 6)

instance HasModifiersFor KeyLocusLastBastion where
  getModifiersFor (KeyLocusLastBastion a) = modifySelf a [IsKeyLocus]

instance HasAbilities KeyLocusLastBastion where
  getAbilities (KeyLocusLastBastion a) =
    [ restricted a 1 (OnSameLocation <> thisExists a AssetWithDamage)
        $ actionAbilityWithCost (ClueCost $ Static 1)
    , mkAbility a 2 $ forced $ AssetDefeated #when ByAny (be a)
    ]

instance RunMessage KeyLocusLastBastion where
  runMessage msg a@(KeyLocusLastBastion attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #willpower (Fixed 2)
      pure a
    PassedThisSkillTestBy _iid (isAbilitySource attrs 1 -> True) n -> do
      healDamage attrs (attrs.ability 1) (1 + n)
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      removeFromGame attrs
      pure a
    _ -> KeyLocusLastBastion <$> liftRunMessage msg attrs
