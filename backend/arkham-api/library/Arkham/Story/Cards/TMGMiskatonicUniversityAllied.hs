module Arkham.Story.Cards.TMGMiskatonicUniversityAllied (
  tmgMiskatonicUniversityAllied,
  TMGMiskatonicUniversityAllied(..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype TMGMiskatonicUniversityAllied = TMGMiskatonicUniversityAllied StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | 'Miskatonic University [seeker]' allied story card.
tmgMiskatonicUniversityAllied :: StoryCard TMGMiskatonicUniversityAllied
tmgMiskatonicUniversityAllied = story TMGMiskatonicUniversityAllied Cards.tmgMiskatonicUniversityAllied

instance HasModifiersFor TMGMiskatonicUniversityAllied where
  getModifiersFor (TMGMiskatonicUniversityAllied attrs) = do
    modifySelect
      attrs
      (enemyIs Enemies.declanPearce <> EnemyWithAsset (assetIs Assets.jewelOfSarnath))
      [CannotBeDamaged, CannotBeDefeated]

instance HasAbilities TMGMiskatonicUniversityAllied where
  getAbilities (TMGMiskatonicUniversityAllied attrs) =
    [ restrictedAbility attrs 1
        (exists $ enemyIs Enemies.declanPearce <> enemyAtLocationWith You)
        parleyAction_
    , restrictedAbility attrs 2
        (exists $ assetIs Assets.jewelOfSarnath <> AssetControlledBy Anyone)
        $ Objective
        $ triggered (RoundEnds #when) Free
    ]

instance RunMessage TMGMiskatonicUniversityAllied where
  runMessage msg s@(TMGMiskatonicUniversityAllied attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      clues <- field InvestigatorClues iid
      chooseAmount' iid "cluesToPlace" "$clues" 0 (min 2 clues) attrs
      pure s
    ResolveAmounts iid (getChoiceAmount "$clues" -> n) (isTarget attrs -> True) | n > 0 -> do
      declan <- selectJust $ enemyIs Enemies.declanPearce <> enemyAtLocationWith iid
      mAid <- selectOne $ assetIs Assets.jewelOfSarnath <> AssetAttachedTo (EnemyWithId declan)
      current <- field EnemyClues declan
      threshold <- perPlayer 3
      pushAll
        [ InvestigatorSpendClues iid n
        , PlaceClues (attrs.ability 1) (EnemyTarget declan) n
        ]
      when (maybe False (const True) mAid && current + n >= threshold) do
        for_ mAid \aid ->
          pushAll [TakeControlOfAsset iid aid, AddToVictory (EnemyTarget declan)]
      pure s
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      actId <- getCurrentAct
      push $ AdvanceAct actId (toSource attrs) #other
      pure s
    _ -> TMGMiskatonicUniversityAllied <$> liftRunMessage msg attrs
