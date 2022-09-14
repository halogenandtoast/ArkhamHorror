module Arkham.Act.Cards.AlejandrosPrison
  ( AlejandrosPrison(..)
  , alejandrosPrison
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes
import Arkham.Criteria
import Arkham.Game.Helpers
import Arkham.Matcher
import Arkham.Message
import Arkham.Resolution
import Arkham.Scenarios.ThreadsOfFate.Helpers
import Arkham.Target

newtype AlejandrosPrison = AlejandrosPrison ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alejandrosPrison :: ActCard AlejandrosPrison
alejandrosPrison = act (3, C) AlejandrosPrison Cards.alejandrosPrison Nothing

instance HasModifiersFor AlejandrosPrison where
  getModifiersFor (LocationTarget lid) (AlejandrosPrison a) = do
    isModified <- lid
      <=~> LocationWithAsset (assetIs Assets.alejandroVela)
    pure $ toModifiers a [ ShroudModifier 2 | isModified ]
  getModifiersFor _ _ = pure []

instance HasAbilities AlejandrosPrison where
  getAbilities (AlejandrosPrison a) =
    [ restrictedAbility
          a
          1
          (LocationExists
          $ LocationWithAsset (assetIs Assets.alejandroVela)
          <> LocationWithoutClues
          )
        $ Objective
        $ ForcedAbility AnyWindow
    | onSide C a
    ]

instance RunMessage AlejandrosPrison where
  runMessage msg a@(AlejandrosPrison attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      push $ AdvanceAct (toId attrs) (toSource attrs) AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == actId attrs && onSide D attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      deckCount <- getActDecksInPlayCount
      alejandroVela <- selectJust $ assetIs Assets.alejandroVela
      iids <- selectList $ NearestToLocation $ LocationWithAsset $ assetIs
        Assets.alejandroVela
      let
        takeControlMessage = chooseOrRunOne
          leadInvestigatorId
          [ targetLabel iid [TakeControlOfAsset iid alejandroVela] | iid <- iids ]
        nextMessage =
          if deckCount <= 1
            then ScenarioResolution $ Resolution 1
            else RemoveFromGame (ActTarget $ toId attrs)
      pushAll [takeControlMessage, nextMessage]
      pure a
    _ -> AlejandrosPrison <$> runMessage msg attrs
