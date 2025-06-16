module Arkham.Story.Cards.TMGTheSyndicateAllied (
  tmgTheSyndicateAllied,
  TMGTheSyndicateAllied(..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype TMGTheSyndicateAllied = TMGTheSyndicateAllied StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tmgTheSyndicateAllied :: StoryCard TMGTheSyndicateAllied
tmgTheSyndicateAllied = story TMGTheSyndicateAllied Cards.tmgTheSyndicateAllied

instance HasModifiersFor TMGTheSyndicateAllied where
  getModifiersFor (TMGTheSyndicateAllied attrs) = do
    modifySelect
      attrs
      (enemyIs Enemies.declanPearce <> EnemyWithAsset (assetIs Assets.jewelOfSarnath))
      [CannotBeDamaged, CannotBeDefeated]

instance HasAbilities TMGTheSyndicateAllied where
  getAbilities (TMGTheSyndicateAllied attrs) =
    [ restrictedAbility attrs 1 Here $ ActionAbility Nothing (ClueCost (Static 1))
    , restrictedAbility attrs 2 (DuringTurn You) $ FastAbility Free
    , restrictedAbility attrs 3
        ( exists
            $ assetIs Assets.jewelOfSarnath
            <> AssetControlledBy (InvestigatorAt $ locationIs Locations.tmgLobby)
        )
        $ Objective
        $ triggered (RoundEnds #when) (GroupClueCost (PerPlayer 4) Anywhere)
    ]

instance RunMessage TMGTheSyndicateAllied where
  runMessage msg s@(TMGTheSyndicateAllied attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ InvestigatorSpendClues iid 1
      pure s
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      pure s
    UseCardAbility _ (isSource attrs -> True) 3 _ _ -> do
      actId <- getCurrentAct
      push $ AdvanceAct actId (toSource attrs) #other
      pure s
    _ -> TMGTheSyndicateAllied <$> liftRunMessage msg attrs
