module Arkham.Act.Cards.BeginnersLuck (beginnersLuck) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Agenda
import Arkham.Helpers.ChaosBag
import Arkham.Investigator.Projection ()
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
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
    guard (onSide A x)
      *> [ groupLimit PerRound $ mkAbility x 1 $ freeReaction (RevealChaosToken #when You AnyChaosToken)
         , mkAbility x 2
             $ Objective
             $ ForcedAbilityWithCost AnyWindow (GroupClueCost (PerPlayer 4) Anywhere)
         ]

instance RunMessage BeginnersLuck where
  runMessage msg a@(BeginnersLuck attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (Window.revealedChaosTokens -> [token]) _ -> do
      chaosTokensInBag <- getBagChaosTokens
      focusChaosTokens chaosTokensInBag \unfocus -> chooseOneM iid do
        targets chaosTokensInBag \token' -> do
          chaosTokenEffect (attrs.ability 1) token $ ChaosTokenFaceModifier [token'.face]
          push unfocus
          push $ FocusChaosTokens [token']
      remember . Cheated =<< iid.labeled
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithClues attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      placeSetAsideLocation_ Locations.darkenedHall
      whenCurrentAgendaStepIs (== 1) do
        lead <- getLead
        discardUntilFirst lead attrs Deck.EncounterDeck $ basic (#enemy <> CardWithTrait Criminal)
      advanceActDeck attrs
      pure a
    RequestedEncounterCard (isSource attrs -> True) _ (Just ec) -> do
      darkenedHallId <- selectJust $ LocationWithTitle "Darkened Hall"
      spawnEnemyAt_ ec darkenedHallId
      pure a
    _ -> BeginnersLuck <$> liftRunMessage msg attrs
