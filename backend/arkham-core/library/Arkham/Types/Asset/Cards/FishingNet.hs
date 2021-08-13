module Arkham.Types.Asset.Cards.FishingNet
  ( FishingNet(..)
  , fishingNet
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Game.Helpers
import Arkham.Types.Id
import Arkham.Types.Keyword
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Restriction
import Arkham.Types.Target

newtype FishingNet = FishingNet AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

fishingNet :: AssetCard FishingNet
fishingNet = assetWith FishingNet Cards.fishingNet (isStoryL .~ True)

instance HasModifiersFor env FishingNet where
  getModifiersFor _ (EnemyTarget eid) (FishingNet attrs) = pure $ toModifiers
    attrs
    [ RemoveKeyword Retaliate | assetEnemy attrs == Just eid ]
  getModifiersFor _ _ _ = pure []

instance HasActions FishingNet where
  getActions (FishingNet x) =
    [restrictedAbility x 1 restriction $ FastAbility Free]
   where
    restriction = case assetEnemy x of
      Just _ -> Never
      Nothing -> OwnsThis <> EnemyExists
        (ExhaustedEnemy <> EnemyAt YourLocation <> enemyIs Cards.theRougarou)

instance AssetRunner env => RunMessage env FishingNet where
  runMessage msg a@(FishingNet attrs@AssetAttrs {..}) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      mrougarou <- fmap unStoryEnemyId <$> getId (CardCode "81028")
      case mrougarou of
        Nothing -> error "can not use this ability"
        Just eid -> a <$ push (AttachAsset assetId (EnemyTarget eid))
    _ -> FishingNet <$> runMessage msg attrs
