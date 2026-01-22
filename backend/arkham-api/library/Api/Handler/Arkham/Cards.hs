module Api.Handler.Arkham.Cards (
  getApiV1ArkhamCardR,
  getApiV1ArkhamCardsR,
) where

import Import

import Arkham.Asset.Cards
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.EncounterCard
import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.PlayerCard
import Arkham.Scenario
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

getApiV1ArkhamCardsR :: Handler [CardDef]
getApiV1ArkhamCardsR = do
  showEncounter <- isJust <$> lookupGetParam "includeEncounter"
  let
    cards =
      if showEncounter
        then
          allInvestigatorCards
            <> allPlayerCards
            <> allEncounterCards
            <> allScenarioCards
            <> allEncounterInvestigatorCards
        else
          allInvestigatorCards
            <> Map.filter (isNothing . cdEncounterSet) allPlayerCards
            <> Map.fromList
              (map (toCardCode &&& id) [Locations.dreamGateWondrousJourney, Events.theStarsAreRight]) -- is encounter back so won't show correctly
    safeBCodes =
      [ "03047b"
      , "03084b"
      , "03276b"
      , "03279b"
      , "07174b"
      , "07175b"
      , "07176b"
      , "07177b"
      , "07204b"
      , "07205b"
      , "07206b"
      , "07207b"
      , "07208b"
      , "07209b"
      , "09586b"
      , "09716b"
      , "09726b"
      , "09727b"
      , "09747b"
      , "09748b"
      , "09749b"
      , "10510b"
      , "10511b"
      , "88035b"
      , "88036b"
      , "88037b"
      , "88038b"
      , "88039b"
      , "88040b"
      , "88041b"
      , "88042b"
      , "88046b"
      , "88047b"
      , "88049b"
      , "88050b"
      , "88051b"
      , "88052b"
      , "88053b"
      ]
    safeDCodes = ["03084d", "88038d"]

  pure
    $ filter
      ( and
          . sequence
            [ (/= "01000")
            , or
                . sequence
                  [(not . T.isSuffixOf "b" . unCardCode), (`elem` safeBCodes)]
            , or
                . sequence
                  [(not . T.isSuffixOf "d" . unCardCode), (`elem` safeDCodes)]
            , (not . T.isSuffixOf "f" . unCardCode)
            , (not . T.isSuffixOf "h" . unCardCode)
            , (not . T.isSuffixOf "j" . unCardCode)
            , (not . T.isSuffixOf "l" . unCardCode)
            ]
          . toCardCode
      )
    $ toList
    $ cards
    `Map.difference` allSpecialPlayerAssetCards

getApiV1ArkhamCardR :: CardCode -> Handler CardDef
getApiV1ArkhamCardR cCode = do
  let allCards =
        allInvestigatorCards
          <> allPlayerCards
          <> allEncounterCards
          <> allScenarioCards
          <> allEncounterInvestigatorCards
  maybe notFound pure $ Map.lookup cCode allCards
