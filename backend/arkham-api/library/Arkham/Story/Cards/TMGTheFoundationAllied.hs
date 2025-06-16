module Arkham.Story.Cards.TMGTheFoundationAllied (
  tmgTheFoundationAllied,
  TMGTheFoundationAllied(..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Message.Lifted
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype TMGTheFoundationAllied = TMGTheFoundationAllied StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | 'The Foundation [guardian]' allied story card (#71015).
-- This side represents working with The Foundation.
tmgTheFoundationAllied :: StoryCard TMGTheFoundationAllied
tmgTheFoundationAllied = story TMGTheFoundationAllied Cards.tmgTheFoundationAllied

instance HasAbilities TMGTheFoundationAllied where
  getAbilities (TMGTheFoundationAllied attrs) =
    [ mkAbility attrs 1
        $ forced
        $ EnemyDefeated #when Anyone ByAny (enemyIs Enemies.theBloodlessManUnleashed)
    , restrictedAbility attrs 2
        ( Here
            <> exists (assetIs Assets.jewelOfSarnath <> assetAtLocationWith You)
            <> notExists (enemyIs Enemies.theBloodlessMan)
            <> notExists (enemyIs Enemies.theBloodlessManUnleashed)
        )
        actionAbility
    , restrictedAbility attrs 3
        (exists $ assetIs Assets.jewelOfSarnath <> AssetControlledBy Anyone)
        $ Objective
        $ triggered (RoundEnds #when) (GroupClueCost (PerPlayer 2) Anywhere)
    ]

instance RunMessage TMGTheFoundationAllied where
  runMessage msg s@(TMGTheFoundationAllied attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (defeatedEnemy -> enemy) _ -> do
      mAid <- selectOne $ assetIs Assets.jewelOfSarnath <> AssetAttachedTo (EnemyWithId enemy)
      for_ mAid \aid -> do
        mLoc <- field EnemyLocation enemy
        for_ mLoc \lid -> push $ AttachAsset aid (LocationTarget lid)
      pure s
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      aid <- selectJust $ assetIs Assets.jewelOfSarnath <> assetAtLocationWith iid
      pushAll
        [ TakeControlOfAsset iid aid
        , PlaceDamage (attrs.ability 2) (AssetTarget aid) 1
        ]
      pure s
    UseCardAbility _ (isSource attrs -> True) 3 _ _ -> do
      advancedWithClues attrs
      pure s
    _ -> TMGTheFoundationAllied <$> liftRunMessage msg attrs
