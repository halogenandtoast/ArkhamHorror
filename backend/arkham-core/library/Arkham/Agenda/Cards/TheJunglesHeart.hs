module Arkham.Agenda.Cards.TheJunglesHeart (TheJunglesHeart (..), theJunglesHeart) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Card
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Helpers.Location
import Arkham.Matcher
import Arkham.Prelude

newtype TheJunglesHeart = TheJunglesHeart AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

theJunglesHeart :: AgendaCard TheJunglesHeart
theJunglesHeart = agenda (1, A) TheJunglesHeart Cards.theJunglesHeart (Static 5)

instance HasAbilities TheJunglesHeart where
  getAbilities (TheJunglesHeart a) = [mkAbility a 1 exploreAction_]

instance RunMessage TheJunglesHeart where
  runMessage msg a@(TheJunglesHeart attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locationSymbols <- toConnections =<< getJustLocation iid
      let source = toAbilitySource attrs 1
      push $ Explore iid source (oneOf $ map CardWithPrintedLocationSymbol locationSymbols)
      pure a
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      lead <- getLeadPlayer
      iids <- getInvestigatorIds
      withBinoculars <- getInvestigatorsWithSupply Binoculars

      let
        lookoutMessages iid =
          if iid `elem` withBinoculars
            then []
            else
              [ DiscardUntilFirst
                  iid
                  (toSource attrs)
                  Deck.EncounterDeck
                  (BasicCardMatch $ CardWithType EnemyType)
              ]

      pushAll
        [ ShuffleEncounterDiscardBackIn
        , chooseOrRunOne lead [targetLabel iid $ lookoutMessages iid | iid <- iids]
        , advanceAgendaDeck attrs
        ]
      pure a
    RequestedEncounterCard (isSource attrs -> True) (Just iid) (Just ec) -> do
      lid <- getJustLocation iid
      push $ SpawnEnemyAt (EncounterCard ec) lid
      pure a
    _ -> TheJunglesHeart <$> runMessage msg attrs
