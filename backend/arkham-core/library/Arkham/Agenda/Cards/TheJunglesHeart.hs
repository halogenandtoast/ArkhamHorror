module Arkham.Agenda.Cards.TheJunglesHeart
  ( TheJunglesHeart(..)
  , theJunglesHeart
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Card
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Helpers.Location
import Arkham.Matcher
import Arkham.Message
import Arkham.Scenario.Deck

newtype TheJunglesHeart = TheJunglesHeart AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theJunglesHeart :: AgendaCard TheJunglesHeart
theJunglesHeart =
  agenda (1, A) TheJunglesHeart Cards.theJunglesHeart (Static 5)

instance HasAbilities TheJunglesHeart where
  getAbilities (TheJunglesHeart a) =
    [ restrictedAbility a 1 (ScenarioDeckWithCard ExplorationDeck)
        $ ActionAbility (Just Action.Explore)
        $ ActionCost 1
    ]

instance RunMessage TheJunglesHeart where
  runMessage msg a@(TheJunglesHeart attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      locationSymbols <- toConnections =<< getJustLocation iid
      push $ Explore
        iid
        source
        (CardWithOneOf $ map CardWithPrintedLocationSymbol locationSymbols)
      pure a
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      leadInvestigator <- getLeadInvestigatorId
      iids <- getInvestigatorIds
      withBinoculars <- getInvestigatorsWithSupply Binoculars

      let
        lookoutMessages iid = if iid `elem` withBinoculars
          then []
          else
            [ DiscardEncounterUntilFirst
                (toSource attrs)
                (Just iid)
                (CardWithType EnemyType)
            ]

      pushAll
        [ ShuffleEncounterDiscardBackIn
        , chooseOrRunOne
          leadInvestigator
          [ targetLabel iid $ lookoutMessages iid | iid <- iids ]
        , AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)
        ]
      pure a
    RequestedEncounterCard (isSource attrs -> True) (Just iid) (Just ec) -> do
      lid <- getJustLocation iid
      push $ SpawnEnemyAt (EncounterCard ec) lid
      pure a
    _ -> TheJunglesHeart <$> runMessage msg attrs
