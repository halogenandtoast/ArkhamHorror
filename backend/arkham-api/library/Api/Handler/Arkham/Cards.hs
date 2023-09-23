module Api.Handler.Arkham.Cards (
  getApiV1ArkhamCardsR,
) where

import Import

import Arkham.Asset.Cards
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.EncounterCard
import Arkham.Investigator.Cards
import Arkham.PlayerCard
import Arkham.Scenario
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

getApiV1ArkhamCardsR :: Handler [CardDef]
getApiV1ArkhamCardsR = do
  showEncounter <-
    maybe False (const True)
      <$> lookupGetParam "includeEncounter"
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
    safeBCodes = ["03047b", "03084b"]
    safeDCodes = ["03084d"]

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
