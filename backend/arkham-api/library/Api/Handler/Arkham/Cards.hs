module Api.Handler.Arkham.Cards (
  getApiV1ArkhamCardR,
  getApiV1ArkhamCardsR,
  getApiV1ArkhamHomebrewCardsR,
  getApiV1ArkhamTraitsR,
  getApiV1ArkhamSchemaR,
) where

import Import

import Arkham.Asset.Cards
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Custom.Schema (customSchema)
import Arkham.EncounterCard
import Arkham.Homebrew.Defs qualified as Homebrew
import Arkham.Investigator.Cards
import Arkham.PlayerCard
import Arkham.Scenario
import Arkham.Trait
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

{- | The card defs to list in the card browser.

Two defs that are the two sides of one physical card would otherwise each get
their own entry, showing the same card twice. Sides are declared with
'cdOtherSide', and we keep only the earlier one — unsuffixed before @a@ before
@b@ — since the other side is reachable by flipping it.

A 'cdOtherSide' pointing at a code that has no def of its own (the usual case,
where the back is only art) hides nothing, and neither do sibling codes such as
@89010a@..@89010i@ that are distinct cards rather than two sides of one.
-}
browsableCardDefs :: Map CardCode CardDef -> [CardDef]
browsableCardDefs defs = filter (\def -> exactCardCode def `Set.notMember` backSides) (toList defs)
 where
  codes :: Set CardCodeExact
  codes = Set.fromList [exactCardCode def | def <- toList defs]

  backSides :: Set CardCodeExact
  backSides =
    Set.fromList
      [ max (exactCardCode def) (exactCardCode otherSide)
      | def <- toList defs
      , Just otherSide <- [cdOtherSide def]
      , exactCardCode otherSide `Set.member` codes
      ]

{- | Blood Token is an encounter card that belongs to no encounter set, so the
usual test would file it with the player cards.
-}
setlessEncounterCards :: Set CardCode
setlessEncounterCards = Set.fromList ["13119"]

isCampaignCardDef :: CardDef -> Bool
isCampaignCardDef def =
  isJust (cdEncounterSet def) || toCardCode def `Set.member` setlessEncounterCards

getApiV1ArkhamCardsR :: Handler [CardDef]
getApiV1ArkhamCardsR = do
  cardPool <- fromMaybe "player" <$> lookupGetParam "cardPool"
  showEncounter <- isJust <$> lookupGetParam "includeEncounter"
  let
    allCards =
      allInvestigatorCards
        <> allPlayerCards
        <> allEncounterCards
        <> allScenarioCards
        <> allEncounterInvestigatorCards
    playerCards = Map.filter (not . isCampaignCardDef) allCards
    campaignCards = Map.filter isCampaignCardDef allCards
    cards = case cardPool of
      "campaign" -> campaignCards
      "both" -> allCards
      _ | showEncounter -> allCards
      _ -> playerCards

  pure
    $ filter ((/= "01000") . toCardCode)
    $ browsableCardDefs
    $ cards
    `Map.difference` allSpecialPlayerAssetCards

getApiV1ArkhamHomebrewCardsR :: Handler [CardDef]
getApiV1ArkhamHomebrewCardsR = do
  let allHomebrewCards =
        Homebrew.locationsMap
          <> Homebrew.enemiesMap
          <> Homebrew.treacheriesMap
          <> Homebrew.playerTreacheriesMap
          <> Homebrew.actsMap
          <> Homebrew.agendasMap
          <> Homebrew.encounterAssetsMap
          <> Homebrew.playerSkillsMap
          <> Homebrew.storiesMap

  pure $ browsableCardDefs allHomebrewCards

getApiV1ArkhamCardR :: CardCode -> Handler CardDef
getApiV1ArkhamCardR cCode = do
  let allCards =
        allInvestigatorCards
          <> allPlayerCards
          <> allEncounterCards
          <> allScenarioCards
          <> allEncounterInvestigatorCards
  maybe notFound pure $ Map.lookup cCode allCards

{- | Every trait the engine knows, as the name it serializes under paired with
how it reads on a card ("AncientOne" / "Ancient One").

The custom card editor takes traits as a printed trait line, so it needs the
display form to recognise what the player typed; anything it cannot match is
sent through as a homebrew trait.
-}
getApiV1ArkhamTraitsR :: Handler Value
getApiV1ArkhamTraitsR = pure $ toJSON [(traitName t, displayTrait t) | t <- Homebrew.allTraits]

{- | The constructor schema the custom-card ability editor builds against.
See "Arkham.Custom.Schema".
-}
getApiV1ArkhamSchemaR :: Handler Value
getApiV1ArkhamSchemaR = pure $ toJSON customSchema
