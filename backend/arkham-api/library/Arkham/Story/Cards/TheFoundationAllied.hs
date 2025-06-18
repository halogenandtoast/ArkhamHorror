module Arkham.Story.Cards.TheFoundationAllied (theFoundationAllied) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Helpers.Location
import Arkham.Helpers.Window (defeatedEnemy)
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted hiding (EnemyDefeated)

newtype TheFoundationAllied = TheFoundationAllied StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | 'The Foundation [guardian]' allied story card (#71015).
This side represents working with The Foundation.
-}
theFoundationAllied :: StoryCard TheFoundationAllied
theFoundationAllied = persistStory $ story TheFoundationAllied Cards.theFoundationAllied

instance HasAbilities TheFoundationAllied where
  getAbilities (TheFoundationAllied a) =
    [ mkAbility a 1
        $ forced
        $ EnemyDefeated #when Anyone ByAny (enemyIs Enemies.theBloodlessManUnleashed)
    , restricted
        a
        2
        ( Here
            <> exists (assetIs Assets.jewelOfSarnath <> AssetAt YourLocation)
            <> notExists (enemyIs Enemies.theBloodlessMan)
            <> notExists (enemyIs Enemies.theBloodlessManUnleashed)
        )
        actionAbility
    , restricted a 3 (exists $ assetIs Assets.jewelOfSarnath <> AssetControlledBy Anyone)
        $ Objective
        $ triggered (RoundEnds #when) (GroupClueCost (PerPlayer 2) Anywhere)
    ]

instance RunMessage TheFoundationAllied where
  runMessage msg s@(TheFoundationAllied attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (defeatedEnemy -> enemy) _ -> do
      mAid <- selectOne $ assetIs Assets.jewelOfSarnath <> AssetAttachedTo (targetIs enemy)
      for_ mAid \aid -> withLocationOf enemy (place aid . AttachedToLocation)
      pure s
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      aid <- selectJust $ assetIs Assets.jewelOfSarnath <> assetAtLocationWith iid
      takeControlOfAsset iid aid
      dealAssetDamage aid (attrs.ability 2) 1
      pure s
    UseCardAbility _ (isSource attrs -> True) 3 _ _ -> do
      advanceCurrentActWithClues attrs
      pure s
    _ -> TheFoundationAllied <$> liftRunMessage msg attrs
