module Arkham.Act.Cards.AlejandrosPlight
  ( AlejandrosPlight(..)
  , alejandrosPlight
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

newtype AlejandrosPlight = AlejandrosPlight ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alejandrosPlight :: ActCard AlejandrosPlight
alejandrosPlight = act (3, C) AlejandrosPlight Cards.alejandrosPlight Nothing

instance HasModifiersFor AlejandrosPlight where
  getModifiersFor (EnemyTarget lid) (AlejandrosPlight a) = do
    isModified <- lid <=~> EnemyWithAsset (assetIs Assets.alejandroVela)
    pure $ toModifiers a [ HealthModifier 2 | isModified ]
  getModifiersFor _ _ = pure []

instance HasAbilities AlejandrosPlight where
  getAbilities (AlejandrosPlight a) =
    [ mkAbility a 1
        $ Objective
        $ ForcedAbility
        $ EnemyLeavesPlay Timing.When
        $ EnemyWithAsset (assetIs Assets.alejandroVela)
    ]

instance RunMessage AlejandrosPlight where
  runMessage msg a@(AlejandrosPlight attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push $ AdvanceAct (toId attrs) (toSource attrs) AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == actId attrs && onSide D attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      deckCount <- getActDecksInPlayCount
      alejandroVela <- selectJust $ assetIs Assets.alejandroVela
      iids <- selectList $ NearestToEnemy $ EnemyWithAsset $ assetIs
        Assets.alejandroVela
      let
        takeControlMessage = chooseOrRunOne
          leadInvestigatorId
          [ targetLabel iid [TakeControlOfAsset iid alejandroVela]
          | iid <- iids
          ]
        nextMessage = if deckCount <= 1
          then ScenarioResolution $ Resolution 1
          else RemoveFromGame (ActTarget $ toId attrs)
      pushAll [takeControlMessage, nextMessage]
      pure a
    _ -> AlejandrosPlight <$> runMessage msg attrs
