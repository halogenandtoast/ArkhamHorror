module Arkham.Act.Cards.RecoverTheRelic (
  RecoverTheRelic (..),
  recoverTheRelic,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes
import Arkham.Matcher
import Arkham.Resolution
import Arkham.Scenarios.ThreadsOfFate.Helpers
import Arkham.Timing qualified as Timing

newtype RecoverTheRelic = RecoverTheRelic ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

recoverTheRelic :: ActCard RecoverTheRelic
recoverTheRelic = act (3, A) RecoverTheRelic Cards.recoverTheRelic Nothing

instance HasModifiersFor RecoverTheRelic where
  getModifiersFor (RecoverTheRelic a) = do
    modifySelect a (EnemyWithAsset $ assetIs Assets.relicOfAgesADeviceOfSomeSort) [HealthModifier 2]

instance HasAbilities RecoverTheRelic where
  getAbilities (RecoverTheRelic a) =
    [ mkAbility a 1
        $ Objective
        $ ForcedAbility
        $ EnemyLeavesPlay Timing.When
        $ EnemyWithAsset (assetIs Assets.relicOfAgesADeviceOfSomeSort)
    ]

instance RunMessage RecoverTheRelic where
  runMessage msg a@(RecoverTheRelic attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push $ AdvanceAct (toId attrs) (toSource attrs) AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == actId attrs && onSide B attrs -> do
      lead <- getLeadPlayer
      deckCount <- getActDecksInPlayCount
      relicOfAges <- selectJust $ assetIs Assets.relicOfAgesADeviceOfSomeSort
      iids <- select $ NearestToEnemy $ EnemyWithAsset $ assetIs Assets.relicOfAgesADeviceOfSomeSort
      let
        takeControlMessage =
          chooseOrRunOne
            lead
            [targetLabel iid [TakeControlOfAsset iid relicOfAges] | iid <- iids]
        nextMessage =
          if deckCount <= 1
            then ScenarioResolution $ Resolution 1
            else RemoveCompletedActFromGame (actDeckId attrs) (toId attrs)
      pushAll [takeControlMessage, nextMessage]
      pure a
    _ -> RecoverTheRelic <$> runMessage msg attrs
