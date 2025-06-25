module Arkham.Agenda.Cards.TheJunglesHeart (theJunglesHeart) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Investigator
import Arkham.Helpers.Location
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype TheJunglesHeart = TheJunglesHeart AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theJunglesHeart :: AgendaCard TheJunglesHeart
theJunglesHeart = agenda (1, A) TheJunglesHeart Cards.theJunglesHeart (Static 5)

instance HasAbilities TheJunglesHeart where
  getAbilities (TheJunglesHeart a) = [mkAbility a 1 exploreAction_]

instance RunMessage TheJunglesHeart where
  runMessage msg a@(TheJunglesHeart attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locationSymbols <- toConnections =<< getJustLocation iid
      push $ Explore iid (attrs.ability 1) (mapOneOf CardWithPrintedLocationSymbol locationSymbols)
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      iids <- getInvestigators
      withBinoculars <- getInvestigatorsWithSupply Binoculars
      shuffleEncounterDiscardBackIn
      leadChooseOrRunOneM $ targets iids \iid -> do
        unless (iid `elem` withBinoculars) do
          discardUntilFirst iid attrs Deck.EncounterDeck #enemy
      advanceAgendaDeck attrs
      pure a
    RequestedEncounterCard (isSource attrs -> True) (Just iid) (Just ec) -> do
      withLocationOf iid $ spawnEnemyAt_ ec
      pure a
    _ -> TheJunglesHeart <$> liftRunMessage msg attrs
