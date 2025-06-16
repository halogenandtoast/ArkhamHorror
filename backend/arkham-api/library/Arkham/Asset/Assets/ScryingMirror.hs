module Arkham.Asset.Assets.ScryingMirror (scryingMirror) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher
import Arkham.Modifier

newtype ScryingMirror = ScryingMirror AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scryingMirror :: AssetCard ScryingMirror
scryingMirror = asset ScryingMirror Cards.scryingMirror

instance HasAbilities ScryingMirror where
  getAbilities (ScryingMirror a) =
    [ restricted a 1 ControlsThis
        $ triggered
          (InitiatedSkillTest #when (colocatedWithMatch You) AnySkillType AnySkillTestValue #any)
          (exhaust a <> assetUseCost a Secret 1)
    ]

instance RunMessage ScryingMirror where
  runMessage msg a@(ScryingMirror attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      modifyCurrentSkillTest attrs RevealChaosTokensBeforeCommittingCards
      pure a
    _ -> ScryingMirror <$> liftRunMessage msg attrs
