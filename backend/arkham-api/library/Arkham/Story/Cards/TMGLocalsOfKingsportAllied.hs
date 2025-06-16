module Arkham.Story.Cards.TMGLocalsOfKingsportAllied (
  tmgLocalsOfKingsportAllied,
  TMGLocalsOfKingsportAllied(..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Token
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype TMGLocalsOfKingsportAllied = TMGLocalsOfKingsportAllied StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tmgLocalsOfKingsportAllied :: StoryCard TMGLocalsOfKingsportAllied
tmgLocalsOfKingsportAllied = story TMGLocalsOfKingsportAllied Cards.tmgLocalsOfKingsportAllied

instance HasAbilities TMGLocalsOfKingsportAllied where
  getAbilities (TMGLocalsOfKingsportAllied attrs) =
    [ mkAbility attrs 1
        $ forced
        $ EnemyDefeated #when Anyone ByAny (enemyIs Enemies.declanPearce)
    , restrictedAbility attrs 2
        (exists $ assetIs Assets.jewelOfSarnath <> assetAtLocationWith You)
        actionAbility
    , restrictedAbility attrs 3
        ( exists (assetIs Assets.jewelOfSarnath <> AssetControlledBy Anyone)
            <> notExists (assetIs Assets.jewelOfSarnath <> AssetWithTokens AnyValue Damage)
            <> notExists (assetIs Assets.jewelOfSarnath <> AssetWithTokens AnyValue Doom)
        )
        $ Objective
        $ triggered (RoundEnds #when) (GroupClueCost (PerPlayer 3) Anywhere)
    ]

instance RunMessage TMGLocalsOfKingsportAllied where
  runMessage msg s@(TMGLocalsOfKingsportAllied attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (defeatedEnemy -> enemy) _ -> do
      mAid <- selectOne $ assetIs Assets.jewelOfSarnath <> AssetAttachedTo (EnemyWithId enemy)
      for_ mAid \aid -> do
        mLoc <- field EnemyLocation enemy
        for_ mLoc \lid -> push $ AttachAsset aid (LocationTarget lid)
      pure s
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      aid <- selectJust $ assetIs Assets.jewelOfSarnath <> assetAtLocationWith iid
      sid <- getRandom
      chooseOne iid
        [ SkillLabel #combat [beginSkillTest sid iid (attrs.ability 2) aid #combat (Fixed 3)]
        , SkillLabel #agility [beginSkillTest sid iid (attrs.ability 2) aid #agility (Fixed 3)]
        ]
      pure s
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      aid <- selectJust $ assetIs Assets.jewelOfSarnath <> assetAtLocationWith iid
      tokens <- field AssetTokens aid
      when (countTokens Damage tokens > 0) do
        push $ RemoveDamage (attrs.ability 2) (AssetTarget aid) 1
      when (countTokens Doom tokens > 0 && countTokens Damage tokens == 0) do
        push $ RemoveDoom (attrs.ability 2) (AssetTarget aid) 1
      push $ TakeControlOfAsset iid aid
      pure s
    UseCardAbility _ (isSource attrs -> True) 3 _ _ -> do
      actId <- getCurrentAct
      push $ AdvanceAct actId (toSource attrs) #other
      pure s
    _ -> TMGLocalsOfKingsportAllied <$> liftRunMessage msg attrs
