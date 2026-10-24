{-# LANGUAGE OverloadedStrings #-}

module Arkham.Custom.OverlaySpec (spec) where

import Arkham.Card.CardCode (CardCode (..))
import Arkham.Card.CardDef (CardDef (..), emptyCardDef)
import Arkham.Card.CardType (CardType (InvestigatorType))
import Arkham.Card.CustomCard (CustomCard (..), arkhamBuildCustomCardCode, registerCustomCards)
import Arkham.Custom.Overlay
import Arkham.Decklist.Type (ArkhamDBDecklist (..))
import Arkham.Id (InvestigatorId (..))
import Arkham.Investigator (decklistInvestigatorError, isKnownInvestigator)
import Arkham.Name (Name)
import Data.Aeson (toJSON)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import GHC.Records (getField)
import Test.Hspec
import Prelude

-- A deck built at arkham.build for an investigator built here is built on one of
-- that site's placeholders -- it cannot know our cards -- and plays under whoever
-- an overlay lays over it. The upgrade path applied no overlay, so the engine was
-- handed the placeholder and threw from inside building it (#5891).
spec :: Spec
spec = describe "a deck built on an arkham.build placeholder" do
  _ <- runIO $ registerCustomCards (Map.fromList [(ownCode, own), (signatureCode, signature)])

  it "is recognized by the placeholder's own code" do
    needsOverlayInvestigator janeDoeDeck `shouldBe` True

  it "is recognized by the pack the deck says the card came from" do
    needsOverlayInvestigator unlistedPlaceholderDeck `shouldBe` True

  it "is not what an ordinary deck looks like" do
    needsOverlayInvestigator printedDeck `shouldBe` False

  it "cannot be seated as it stands" do
    decklistInvestigatorError janeDoeDeck `shouldSatisfy` isJust

  it "plays as whoever is laid over it, with their signatures" do
    let played = applyOverlay (investigatorOverlay (InvestigatorId ownCode)) janeDoeDeck
    getField @"investigator" played `shouldBe` InvestigatorId ownCode
    Map.lookup signatureCode (slots played) `shouldBe` Just 1
    decklistInvestigatorError played `shouldBe` Nothing

  -- An alternate front outranks the deck's own investigator, so one left in place
  -- would hand back the investigator the overlay just swapped away.
  it "does not keep the alternate front it was swapped away from" do
    let played = applyOverlay (investigatorOverlay (InvestigatorId ownCode)) alternateFrontDeck
    getField @"investigator" played `shouldBe` InvestigatorId ownCode

  describe "isKnownInvestigator" do
    it "knows a printed investigator" do
      isKnownInvestigator "01001" `shouldBe` True

    it "knows a custom one in the registry" do
      isKnownInvestigator (InvestigatorId ownCode) `shouldBe` True

    it "does not know a placeholder, which no registry holds" do
      isKnownInvestigator (InvestigatorId janeDoeCode) `shouldBe` False

ownCode :: CardCode
ownCode = "*0ver1a1d00000000000000000000000"

signatureCode :: CardCode
signatureCode = "*0ver1a1d51gnature0000000000000"

own :: CustomCard
own =
  CustomCard
    (emptyCardDef ownCode ("Overlaid" :: Name) InvestigatorType)
      { cdMeta = Map.fromList [("_signatures", toJSON [signatureCode])]
      }
    Nothing

signature :: CustomCard
signature = CustomCard (emptyCardDef signatureCode ("Overlaid's Own" :: Name) InvestigatorType) Nothing

janeDoeCode :: CardCode
janeDoeCode = arkhamBuildCustomCardCode "c13c4114-0769-410c-864b-e43f5a596c0d"

decklist :: ArkhamDBDecklist
decklist =
  ArkhamDBDecklist
    { slots = Map.empty
    , sideSlots = Map.empty
    , investigator_code = "01001"
    , investigator_name = "Roland Banks"
    , meta = Nothing
    , taboo_id = Nothing
    , url = Nothing
    , decklist_id = Nothing
    , decklist_name = Nothing
    }

janeDoeDeck :: ArkhamDBDecklist
janeDoeDeck =
  decklist
    { investigator_code = InvestigatorId janeDoeCode
    , investigator_name = "Jane Doe"
    , slots = Map.fromList [("01087", 2)]
    }

{- | As the deck arrives from arkham.build: the card is named by an id nothing
here lists, and only the pack it came from says what it is.
-}
unlistedPlaceholderDeck :: ArkhamDBDecklist
unlistedPlaceholderDeck =
  decklist
    { investigator_code =
        InvestigatorId (arkhamBuildCustomCardCode "00000000-0000-0000-0000-000000000001")
    , investigator_name = "Someone Nobody"
    , meta =
        Just
          "{\"fan_made_content\":{\"cards\":{\"00000000-0000-0000-0000-000000000001\":{\"pack_code\":\"d12a4d9c-8c65-4df0-be0c-c2e14af65a21\"}}}}"
    }

printedDeck :: ArkhamDBDecklist
printedDeck = decklist {slots = Map.fromList [("01020", 1)]}

alternateFrontDeck :: ArkhamDBDecklist
alternateFrontDeck =
  janeDoeDeck {meta = Just "{\"alternate_front\":\"*c13c41140769410c864be43f5a596c0d0\"}"}
