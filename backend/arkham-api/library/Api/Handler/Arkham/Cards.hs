module Api.Handler.Arkham.Cards
  ( getApiV1ArkhamCardsR
  ) where

import Import

import Arkham.Asset.Cards
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.EncounterCard
import Arkham.Investigator.Cards
import Arkham.PlayerCard
import Arkham.Scenario
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T

getApiV1ArkhamCardsR :: Handler [SomeCardDef]
getApiV1ArkhamCardsR = do
  showEncounter <- maybe False (const True)
    <$> lookupGetParam "includeEncounter"
  let
    cards = if showEncounter
      then
        HashMap.map toCardDef allInvestigatorCards
        <> HashMap.map toCardDef allPlayerCards
        <> HashMap.map toCardDef allEncounterCards
        <> HashMap.map toCardDef allScenarioCards
        <> HashMap.map toCardDef allEncounterInvestigatorCards
      else
        HashMap.map toCardDef allInvestigatorCards
          <> HashMap.filter (isNothing . withCardDef cdEncounterSet) allPlayerCards
    safeBCodes = ["03047b", "03084b"]
    safeDCodes = ["03084d"]

  pure
    $ filter
        (and
        . sequence
            [ (/= "01000")
            , or . sequence
              [(not . T.isSuffixOf "b" . unCardCode), (`elem` safeBCodes)]
            , or . sequence
              [(not . T.isSuffixOf "d" . unCardCode), (`elem` safeDCodes)]
            , (not . T.isSuffixOf "f" . unCardCode)
            ]
        . toCardCode
        )
    $ toList
    $ cards
    `HashMap.difference` allSpecialPlayerAssetCards
