module Arkham.Act.Cards.StrangeOccurences
  ( StrangeOccurences(..)
  , strangeOccurences
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes
import Arkham.Criteria
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.History
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenarios.ThreadsOfFate.Helpers
import Arkham.Target
import Arkham.Treachery.Types

newtype StrangeOccurences = StrangeOccurences ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangeOccurences :: ActCard StrangeOccurences
strangeOccurences =
  act (3, E) StrangeOccurences Cards.strangeOccurences Nothing

instance HasModifiersFor StrangeOccurences where
  getModifiersFor (TreacheryTarget tid) (StrangeOccurences a) = do
    iid <- field TreacheryDrawnBy tid
    atIchtacasDestination <-
      selectAny $ locationWithInvestigator iid <> IsIchtacasDestination
    treacheriesDrawnCount <-
      length . historyTreacheriesDrawn <$> getHistory TurnHistory iid
    pure $ toModifiers
      a
      [ AddKeyword Keyword.Surge
      | atIchtacasDestination && treacheriesDrawnCount == 1
      ]
  getModifiersFor _ _ = pure []

instance HasAbilities StrangeOccurences where
  getAbilities (StrangeOccurences a) =
    [ restrictedAbility
          a
          1
          (Negate
          $ LocationExists
          $ LocationWithAnyClues
          <> IsIchtacasDestination
          )
        $ Objective
        $ ForcedAbility AnyWindow
    ]

instance RunMessage StrangeOccurences where
  runMessage msg a@(StrangeOccurences attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) _ 1 _ -> do
      push $ AdvanceAct (toId attrs) (toSource attrs) AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == actId attrs && onSide F attrs -> do
      deckCount <- getActDecksInPlayCount
      leadInvestigatorId <- getLeadInvestigatorId
      isTownHall <-
        selectAny $ locationIs Locations.townHall <> IsIchtacasDestination
      ichtaca <- selectJust $ assetIs Assets.ichtacaTheForgottenGuardian
      iids <- selectList $ NearestToLocation $ locationIs $ if isTownHall
        then Locations.townHall
        else Locations.rivertown
      let
        takeControlMessage = chooseOrRunOne
          leadInvestigatorId
          [ targetLabel iid [TakeControlOfAsset iid ichtaca] | iid <- iids ]
        nextMessage = if deckCount <= 1
          then ScenarioResolution $ Resolution 1
          else RemoveFromGame (ActTarget $ toId attrs)
      pushAll [takeControlMessage, nextMessage]
      pure a
    _ -> StrangeOccurences <$> runMessage msg attrs
