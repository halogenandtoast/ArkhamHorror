module Arkham.Asset.Assets.ElleRubashPurifyingPurpose2 (
  elleRubashPurifyingPurpose2,
  ElleRubashPurifyingPurpose2 (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified_, modifySelect)
import Arkham.Helpers.SkillTest (getSkillTestSource)
import Arkham.Matcher
import Arkham.Placement

newtype ElleRubashPurifyingPurpose2 = ElleRubashPurifyingPurpose2 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

elleRubashPurifyingPurpose2 :: AssetCard ElleRubashPurifyingPurpose2
elleRubashPurifyingPurpose2 = ally ElleRubashPurifyingPurpose2 Cards.elleRubashPurifyingPurpose2 (1, 2)

instance HasModifiersFor ElleRubashPurifyingPurpose2 where
  getModifiersFor (ElleRubashPurifyingPurpose2 a) = case a.controller of
    Nothing -> pure mempty
    Just iid -> do
      controller <- maybeModified_ a iid do
        source <- MaybeT getSkillTestSource
        skillTestAsset <- hoistMaybe source.asset
        liftGuardM $ skillTestAsset <=~> AssetAttachedToAsset (be a)
        pure [AnySkillValue 1]
      assets <- modifySelect a (AssetAttachedToAsset (be a)) [IgnoreDoomOnThis 1]
      pure $ controller <> assets

instance HasAbilities ElleRubashPurifyingPurpose2 where
  getAbilities (ElleRubashPurifyingPurpose2 x) =
    [ controlledAbility x 1 (exists $ AssetWithAnyDoom <> AssetInPlayAreaOf You <> not_ (be x))
        $ FastAbility (exhaust x)
    ]

instance RunMessage ElleRubashPurifyingPurpose2 where
  runMessage msg a@(ElleRubashPurifyingPurpose2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assets <- select $ AssetWithAnyDoom <> assetInPlayAreaOf iid <> not_ (be attrs)
      chooseOneToHandle iid (attrs.ability 1) assets
      pure a
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (AssetTarget aid) -> do
      attached <- select $ assetAttachedToAsset attrs.id
      let mustSwap = length attached >= 2
      let placeAsset = PlaceAsset aid (AttachedToAsset attrs.id (Just $ InPlayArea iid))
      chooseOrRunOne iid
        $ [Label "Attach without swapping" [placeAsset] | not mustSwap]
        <> [ targetLabel otherAsset [placeAsset, PlaceAsset otherAsset (InPlayArea iid)] | otherAsset <- attached
           ]
      pure a
    _ -> ElleRubashPurifyingPurpose2 <$> liftRunMessage msg attrs
