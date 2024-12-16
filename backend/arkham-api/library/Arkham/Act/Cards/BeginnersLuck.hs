module Arkham.Act.Cards.BeginnersLuck (BeginnersLuck (..), beginnersLuck) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Agenda
import Arkham.Helpers.ChaosBag
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.ScenarioLogKey
import Arkham.Trait
import Arkham.Window qualified as Window

newtype BeginnersLuck = BeginnersLuck ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- Advancement is forced
beginnersLuck :: ActCard BeginnersLuck
beginnersLuck = act (1, A) BeginnersLuck Cards.beginnersLuck Nothing

instance HasAbilities BeginnersLuck where
  getAbilities (BeginnersLuck x) =
    extend x
      $ guard (onSide A x)
      *> [ groupLimit PerRound $ mkAbility x 1 $ freeReaction (RevealChaosToken #when Anyone AnyChaosToken)
         , mkAbility x 2
            $ Objective
            $ ForcedAbilityWithCost AnyWindow (GroupClueCost (PerPlayer 4) Anywhere)
         ]

instance RunMessage BeginnersLuck where
  runMessage msg a@(BeginnersLuck attrs) = runQueueT $ case msg of
    UseCardAbility iid source 1 (Window.revealedChaosTokens -> [token]) _ | isSource attrs source -> do
      chaosTokensInBag <- getBagChaosTokens
      push $ FocusChaosTokens chaosTokensInBag
      chooseOneM iid do
        targets chaosTokensInBag \token' -> do
          chaosTokenEffect source token $ ChaosTokenFaceModifier [token'.face]
          push UnfocusChaosTokens
          push $ FocusChaosTokens [token']
      remember Cheated
      pure a
    UseCardAbility _ source 2 _ _ | isSource attrs source -> do
      push $ AdvanceAct (toId a) source AdvancedWithClues
      pure a
    AdvanceAct aid _ _ | aid == toId a && onSide B attrs -> do
      lead <- getLead
      isAgenda1 <- (== 1) <$> getCurrentAgendaStep
      placeSetAsideLocation_ Locations.darkenedHall
      when isAgenda1 do
        discardUntilFirst lead attrs Deck.EncounterDeck $ basic (#enemy <> CardWithTrait Criminal)
      advanceActDeck attrs
      pure a
    RequestedEncounterCard source _ (Just ec) | isSource attrs source -> do
      darkenedHallId <- selectJust $ LocationWithTitle "Darkened Hall"
      push $ SpawnEnemyAt (EncounterCard ec) darkenedHallId
      pure a
    _ -> BeginnersLuck <$> liftRunMessage msg attrs
