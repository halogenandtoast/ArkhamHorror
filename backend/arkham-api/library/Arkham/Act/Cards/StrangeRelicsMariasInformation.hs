module Arkham.Act.Cards.StrangeRelicsMariasInformation (strangeRelicsMariasInformation) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.Name
import Arkham.ScenarioLogKey

newtype StrangeRelicsMariasInformation = StrangeRelicsMariasInformation ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangeRelicsMariasInformation :: ActCard StrangeRelicsMariasInformation
strangeRelicsMariasInformation =
  act (2, E) StrangeRelicsMariasInformation Cards.strangeRelicsMariasInformation Nothing

instance HasAbilities StrangeRelicsMariasInformation where
  getAbilities = actAbilities1' E \a ->
    restricted a 1 (exists $ assetIs Assets.mariaDeSilva <> AssetWithClues (AtLeast $ PerPlayer 1))
      $ Objective
      $ forced AnyWindow

instance RunMessage StrangeRelicsMariasInformation where
  runMessage msg a@(StrangeRelicsMariasInformation attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide F attrs -> True) _ _ -> do
      downtown <- selectJust $ locationIs Locations.downtownFirstBankOfArkham
      rivertown <- selectJust $ locationIs Locations.rivertown
      remember $ IchtacasDestination $ Labeled (toName Locations.downtownFirstBankOfArkham) downtown
      remember $ IchtacasDestination $ Labeled (toName Locations.rivertown) rivertown
      eachInvestigator \iid -> discardTopOfEncounterDeckAndHandle iid attrs 1 attrs
      advanceToAct attrs Acts.strangeOccurences E
      pure a
    DiscardedTopOfEncounterDeck iid [card] _ (isTarget attrs -> True) -> do
      when (toCardType card == TreacheryType) $ drawCardFrom iid card Deck.EncounterDeck
      pure a
    _ -> StrangeRelicsMariasInformation <$> liftRunMessage msg attrs
