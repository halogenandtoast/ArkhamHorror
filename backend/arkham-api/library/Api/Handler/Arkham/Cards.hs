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
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T

getApiV1ArkhamCardsR :: Handler [CardDef]
getApiV1ArkhamCardsR = do
  showEncounter <- maybe False (const True)
    <$> lookupGetParam "includeEncounter"
  let
    cards = if showEncounter
      then allInvestigatorCards <> allPlayerCards <> allEncounterCards
      else
        allInvestigatorCards
          <> HashMap.filter (isNothing . cdEncounterSet) allPlayerCards
  pure
    $ filter
        (and
        . sequence [(/= "01000"), (not . T.isSuffixOf "b" . unCardCode)]
        . toCardCode
        )
    $ toList
    $ cards
    `HashMap.difference` allSpecialPlayerAssetCards
