module Arkham.Asset.Assets.FishingNet (fishingNet) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Keyword
import Arkham.Matcher
import Arkham.Placement

newtype FishingNet = FishingNet AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fishingNet :: AssetCard FishingNet
fishingNet = asset FishingNet Cards.fishingNet

instance HasModifiersFor FishingNet where
  getModifiersFor (FishingNet a) = case a.placement of
    AttachedToEnemy eid -> modified_ a eid [RemoveKeyword Retaliate]
    _ -> pure ()

instance HasAbilities FishingNet where
  getAbilities (FishingNet x) =
    [ controlled x 1 (exists $ #exhausted <> at_ YourLocation <> enemyIs Cards.theRougarou)
        $ FastAbility Free
    ]

instance RunMessage FishingNet where
  runMessage msg a@(FishingNet attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectForMaybeM (enemyIs Cards.theRougarou) (attach attrs)
      pure a
    _ -> FishingNet <$> liftRunMessage msg attrs
