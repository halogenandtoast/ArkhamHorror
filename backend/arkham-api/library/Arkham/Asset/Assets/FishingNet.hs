module Arkham.Asset.Assets.FishingNet (FishingNet (..), fishingNet) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Keyword
import Arkham.Matcher
import Arkham.Prelude

newtype FishingNet = FishingNet AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fishingNet :: AssetCard FishingNet
fishingNet = assetWith FishingNet Cards.fishingNet (isStoryL .~ True)

instance HasModifiersFor FishingNet where
  getModifiersFor (EnemyTarget eid) (FishingNet attrs) = do
    toModifiers attrs [RemoveKeyword Retaliate | attachedToEnemy attrs eid]
  getModifiersFor _ _ = pure []

instance HasAbilities FishingNet where
  getAbilities (FishingNet x) =
    [ controlledAbility x 1 (exists $ ExhaustedEnemy <> EnemyAt YourLocation <> enemyIs Cards.theRougarou)
        $ FastAbility Free
    ]

instance RunMessage FishingNet where
  runMessage msg a@(FishingNet attrs@AssetAttrs {..}) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      mrougarou <- selectOne $ enemyIs Cards.theRougarou
      case mrougarou of
        Nothing -> error "can not use this ability"
        Just eid -> a <$ push (AttachAsset assetId (EnemyTarget eid))
    _ -> FishingNet <$> runMessage msg attrs
