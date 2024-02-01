module Arkham.Act.Cards.StrangeOccurences (
  StrangeOccurences (..),
  strangeOccurences,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Classes
import Arkham.Deck qualified as Deck
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.History
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Projection
import Arkham.Scenarios.ThreadsOfFate.Helpers
import Arkham.Treachery.Types

newtype StrangeOccurences = StrangeOccurences ActAttrs
  deriving anyclass (IsAct)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

strangeOccurences :: ActCard StrangeOccurences
strangeOccurences =
  act (3, E) StrangeOccurences Cards.strangeOccurences Nothing

instance HasModifiersFor StrangeOccurences where
  getModifiersFor (TreacheryTarget tid) (StrangeOccurences a) = do
    mDrawnFrom <- field TreacheryDrawnFrom tid
    case mDrawnFrom of
      Just Deck.EncounterDeck -> do
        iid <- field TreacheryDrawnBy tid
        atIchtacasDestination <-
          selectAny $ locationWithInvestigator iid <> IsIchtacasDestination
        treacheriesDrawnCount <-
          length . historyTreacheriesDrawn <$> getHistory TurnHistory iid
        pure
          $ toModifiers
            a
            [ AddKeyword Keyword.Surge
            | atIchtacasDestination && treacheriesDrawnCount == 1
            ]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities StrangeOccurences where
  getAbilities (StrangeOccurences a) =
    [ restrictedAbility
      a
      1
      (AllLocationsMatch IsIchtacasDestination LocationWithoutClues)
      $ Objective
      $ ForcedAbility AnyWindow
    | onSide E a
    ]

instance RunMessage StrangeOccurences where
  runMessage msg a@(StrangeOccurences attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ AdvanceAct (toId attrs) (toSource attrs) AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == actId attrs && onSide F attrs -> do
      deckCount <- getActDecksInPlayCount
      lead <- getLeadPlayer
      isTownHall <- selectAny $ locationIs Locations.townHall <> IsIchtacasDestination
      ichtaca <- genCard Assets.ichtacaTheForgottenGuardian
      iids <-
        selectList
          $ NearestToLocation
          $ locationIs
          $ if isTownHall
            then Locations.townHall
            else Locations.rivertown
      let
        takeControlMessage =
          chooseOrRunOne
            lead
            [ targetLabel iid [TakeControlOfSetAsideAsset iid ichtaca]
            | iid <- iids
            ]
        nextMessage =
          if deckCount <= 1
            then scenarioResolution 1
            else RemoveCompletedActFromGame (actDeckId attrs) (toId attrs)
      pushAll [takeControlMessage, nextMessage]
      pure a
    _ -> StrangeOccurences <$> runMessage msg attrs
