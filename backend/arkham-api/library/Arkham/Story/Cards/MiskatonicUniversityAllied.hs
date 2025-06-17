module Arkham.Story.Cards.MiskatonicUniversityAllied (miskatonicUniversityAllied) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.GameValue
import Arkham.Helpers.Modifiers
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Target

newtype MiskatonicUniversityAllied = MiskatonicUniversityAllied StoryAttrs
  deriving anyclass IsStory
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | 'Miskatonic University [seeker]' allied story card.
miskatonicUniversityAllied :: StoryCard MiskatonicUniversityAllied
miskatonicUniversityAllied = story MiskatonicUniversityAllied Cards.miskatonicUniversityAllied

instance HasModifiersFor MiskatonicUniversityAllied where
  getModifiersFor (MiskatonicUniversityAllied attrs) = do
    modifySelect
      attrs
      (enemyIs Enemies.declanPearce <> EnemyWithAsset (assetIs Assets.jewelOfSarnath))
      [CannotBeDamaged, CannotBeDefeated]

instance HasAbilities MiskatonicUniversityAllied where
  getAbilities (MiskatonicUniversityAllied attrs) =
    [ restricted attrs 1 (exists $ enemyIs Enemies.declanPearce <> EnemyAt YourLocation) parleyAction_
    , restricted attrs 2 (exists $ assetIs Assets.jewelOfSarnath <> AssetControlledBy Anyone)
        $ Objective
        $ freeReaction (RoundEnds #when)
    ]

instance RunMessage MiskatonicUniversityAllied where
  runMessage msg s@(MiskatonicUniversityAllied attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      clues <- field InvestigatorClues iid
      withI18n $ chooseAmount' iid "cluesToPlace" "$clues" 0 (min 2 clues) attrs
      pure s
    ResolveAmounts iid (getChoiceAmount "$clues" -> n) (isTarget attrs -> True) | n > 0 -> do
      spendClues iid n
      declan <- selectJust $ enemyIs Enemies.declanPearce <> enemyAtLocationWith iid
      placeClues (attrs.ability 1) declan n
      mAid <- selectOne $ assetIs Assets.jewelOfSarnath <> AssetAttachedTo (targetIs declan)
      for_ mAid \aid -> do
        current <- field EnemyClues declan
        threshold <- perPlayer 3
        when (current + n >= threshold) do
          takeControlOfAsset iid aid
          addToVictory declan
      pure s
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advanceCurrentAct (attrs.ability 2)
      pure s
    _ -> MiskatonicUniversityAllied <$> liftRunMessage msg attrs
