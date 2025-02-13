module Arkham.Asset.Assets.ArtisticInspiration (artisticInspiration) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype ArtisticInspiration = ArtisticInspiration AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

artisticInspiration :: AssetCard ArtisticInspiration
artisticInspiration = asset ArtisticInspiration Cards.artisticInspiration

instance HasAbilities ArtisticInspiration where
  getAbilities (ArtisticInspiration a) =
    [ controlled a 1 (thisExists a AssetNotAtUsesX) $ FastAbility (exhaust a <> HandDiscardCost 1 #any)
    , restricted a 2 ControlsThis
        $ triggered
          (RevealChaosTokensDuringSkillTest #after You (YourSkillTest AnySkillTest) #any)
          (assetUseCost a Inspiration 1)
    ]

instance RunMessage ArtisticInspiration where
  runMessage msg a@(ArtisticInspiration attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      pure . ArtisticInspiration $ attrs & tokensL %~ replenish Evidence 1
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid -> do
        chooseOneM iid do
          labeled "Get +1" $ skillTestModifier sid (attrs.ability 2) iid (AnySkillValue 1)
          labeled "Get -1" $ skillTestModifier sid (attrs.ability 2) iid (AnySkillValue (-1))
      pure a
    _ -> ArtisticInspiration <$> liftRunMessage msg attrs
