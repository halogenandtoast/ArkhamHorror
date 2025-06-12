module Arkham.Act.Cards.StrangeOccurences (strangeOccurences) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Deck qualified as Deck
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers
import Arkham.History
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.ThreadsOfFate.Helpers
import Arkham.Treachery.Types

newtype StrangeOccurences = StrangeOccurences ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangeOccurences :: ActCard StrangeOccurences
strangeOccurences = act (3, E) StrangeOccurences Cards.strangeOccurences Nothing

instance HasModifiersFor StrangeOccurences where
  getModifiersFor (StrangeOccurences a) = do
    modifySelectMaybe a AnyTreachery \tid -> do
      Deck.EncounterDeck <- MaybeT $ field TreacheryDrawnFrom tid
      iid <- lift $ field TreacheryDrawnBy tid
      liftGuardM $ selectAny $ locationWithInvestigator iid <> IsIchtacasDestination
      liftGuardM $ (== 1) . length . historyTreacheriesDrawn <$> getHistory TurnHistory iid
      pure [AddKeyword Keyword.Surge]

instance HasAbilities StrangeOccurences where
  getAbilities = actAbilities1' E \a ->
    restricted a 1 (AllLocationsMatch IsIchtacasDestination LocationWithoutClues)
      $ Objective
      $ forced AnyWindow

instance RunMessage StrangeOccurences where
  runMessage msg a@(StrangeOccurences attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide F attrs -> True) _ _ -> do
      deckCount <- getActDecksInPlayCount
      isTownHall <- selectAny $ locationIs Locations.townHall <> IsIchtacasDestination
      ichtaca <- genCard Assets.ichtacaTheForgottenGuardian
      iids <-
        select
          $ NearestToLocation
          $ locationIs
          $ if isTownHall then Locations.townHall else Locations.rivertown
      leadChooseOrRunOneM $ targets iids (`takeControlOfSetAsideAsset` ichtaca)
      push
        $ if deckCount <= 1
          then R1
          else RemoveCompletedActFromGame (actDeckId attrs) (toId attrs)
      pure a
    _ -> StrangeOccurences <$> liftRunMessage msg attrs
