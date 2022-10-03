module Arkham.Act.Cards.RecoverTheRelic
  ( RecoverTheRelic(..)
  , recoverTheRelic
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Matcher
import Arkham.Message
import Arkham.Resolution
import Arkham.Scenarios.ThreadsOfFate.Helpers
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype RecoverTheRelic = RecoverTheRelic ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

recoverTheRelic :: ActCard RecoverTheRelic
recoverTheRelic = act (3, A) RecoverTheRelic Cards.recoverTheRelic Nothing

instance HasModifiersFor RecoverTheRelic where
  getModifiersFor (EnemyTarget lid) (RecoverTheRelic a) = do
    isModified <- lid
      <=~> EnemyWithAsset (assetIs Assets.relicOfAgesADeviceOfSomeSort)
    pure $ toModifiers a [ HealthModifier 2 | isModified ]
  getModifiersFor _ _ = pure []

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
      leadInvestigatorId <- getLeadInvestigatorId
      deckCount <- getActDecksInPlayCount
      relicOfAges <- selectJust $ assetIs Assets.relicOfAgesADeviceOfSomeSort
      iids <- selectList $ NearestToEnemy $ EnemyWithAsset $ assetIs
        Assets.relicOfAgesADeviceOfSomeSort
      let
        takeControlMessage = chooseOrRunOne
          leadInvestigatorId
          [ targetLabel iid [TakeControlOfAsset iid relicOfAges] | iid <- iids ]
        nextMessage = if deckCount <= 1
          then ScenarioResolution $ Resolution 1
          else RemoveFromGame (ActTarget $ toId attrs)
      pushAll [takeControlMessage, nextMessage]
      pure a
    _ -> RecoverTheRelic <$> runMessage msg attrs
