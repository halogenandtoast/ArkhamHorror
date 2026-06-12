module Arkham.Scenarios.AllOrNothing.Helpers where

import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Helpers.Campaign (getCampaignStoryCards, matchingCardsAlreadyInDeck)
import Arkham.Helpers.Scenario
import Arkham.I18n
import Arkham.Id
import Arkham.Matcher.Base (mapOneOf)
import Arkham.Matcher.Card (CardMatcher (CardWithCardCode))
import Arkham.Matcher.Investigator
import Arkham.Prelude
import Arkham.Tracing

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = withI18n $ standaloneI18n "allOrNothing" a

skidsOToole :: InvestigatorMatcher
skidsOToole = InvestigatorWithTitle "\"Skids\" O'Toole"

hasCardInDeck :: (HasGame m, Tracing m) => InvestigatorId -> CardDef -> m Bool
hasCardInDeck iid def = do
  let codes = toCardCode def : cdAlternateCardCodes def
  deckCards <- matchingCardsAlreadyInDeck (mapOneOf CardWithCardCode codes)
  storyCards <- getCampaignStoryCards
  pure
    $ maybe False notNull (lookup iid deckCards)
    || maybe False (any ((== def) . toCardDef)) (lookup iid storyCards)
