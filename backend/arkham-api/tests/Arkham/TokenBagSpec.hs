{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PatternSynonyms #-}

module Arkham.TokenBagSpec (spec) where

import Arkham.ChaosToken.Types
import Arkham.Difficulty
import Arkham.Homebrew.CircusExMortis.Helpers (drawFuryBagTokens)
import Arkham.Homebrew.CircusExMortis.Tokens (pattern MoonToken)
import Arkham.Scenario.Types (ScenarioAttrs (..), scenario)
import Arkham.TokenBag
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.UUID qualified as UUID
import Test.Hspec
import Prelude

skull, tablet, cultist :: BagToken
skull = BagToken (ChaosTokenId UUID.nil) Skull
tablet = BagToken (ChaosTokenId $ UUID.fromWords 0 0 0 1) Tablet
cultist = BagToken (ChaosTokenId $ UUID.fromWords 0 0 0 2) Cultist

legacyToken :: String -> BagToken -> Value
legacyToken prefix (BagToken tokenId face) =
  object
    [Key.fromString (prefix <> "TokenId") .= tokenId, Key.fromString (prefix <> "TokenFace") .= face]

legacyBag :: String -> Value
legacyBag prefix =
  object
    [ Key.fromString (prefix <> "Tokens") .= [legacyToken prefix skull]
    , Key.fromString (prefix <> "SetAside") .= [legacyToken prefix tablet]
    , Key.fromString (prefix <> "CurrentToken") .= legacyToken prefix cultist
    ]

expectDecoded :: FromJSON a => Value -> IO a
expectDecoded value = case fromJSON value of
  Success result -> pure result
  Error message -> fail message

expectObject :: Value -> IO Object
expectObject (Object value) = pure value
expectObject _ = fail "Expected a JSON object"

expectJust :: Maybe a -> IO a
expectJust = maybe (fail "Expected Just") pure

spec :: Spec
spec = describe "TokenBag" do
  describe "save compatibility" do
    let expected = (newTokenBag [skull]) {bagSetAside = [tablet], bagCurrentToken = Just cultist}
    it "loads the original Infestation bag and token fields without losing IDs or piles" do
      fromJSON (legacyBag "infestation") `shouldBe` Success expected
    it "loads the original Predation fields with a missing cancel flag" do
      fromJSON (legacyBag "predation") `shouldBe` Success expected
    it "preserves an existing Predation cancellation" do
      old <- expectObject $ legacyBag "predation"
      fromJSON (Object $ KeyMap.insert "predationCancelNext" (Bool True) old)
        `shouldBe` Success expected {bagCancelNext = True}
    it "loads empty legacy bags" do
      let value =
            object
              [ "infestationTokens" .= ([] :: [Value])
              , "infestationSetAside" .= ([] :: [Value])
              , "infestationCurrentToken" .= Null
              ]
      fromJSON value `shouldBe` Success (newTokenBag [] :: CustomChaosBag)
    it "round-trips all new state, including cancellation and debug overrides" do
      let bag = expected {bagCancelNext = True, bagDebugNext = Just Skull}
      decode (encode bag) `shouldBe` Just bag
    it "writes only the shared token format after loading a legacy token" do
      fmap toJSON (fromJSON (legacyToken "predation" skull) :: Result BagToken)
        `shouldBe` Success (toJSON skull)
    it "does not accept Fury's temporary face-only format" do
      (fromJSON (toJSON [Skull]) :: Result CustomChaosBag) `shouldSatisfy` \case
        Error _ -> True
        Success _ -> False

  describe "shared operations" do
    it "draws an existing token without replacement" do
      (drawn, bag) <- drawBagToken bagTokenFace $ newTokenBag [skull]
      drawn `shouldBe` Just skull
      bagTokens bag `shouldBe` []
      bagCurrentToken bag `shouldBe` Just skull
    it "removes only one copy of a repeated face" do
      let second = tablet {bagTokenFace = Skull}
      (drawn, bag) <- drawBagToken bagTokenFace $ newTokenBag [skull, second]
      length (bagTokens bag) `shouldBe` 1
      sort (allBagTokens bag) `shouldBe` sort [skull, second]
      drawn `shouldNotBe` Nothing
    it "does not overwrite a pending reveal" do
      let bag = (newTokenBag [skull]) {bagCurrentToken = Just tablet}
      drawBagToken bagTokenFace bag `shouldReturn` (Nothing, bag)
    it "handles an empty bag" do
      let bag = newTokenBag [] :: CustomChaosBag
      drawBagToken bagTokenFace bag `shouldReturn` (Nothing, bag)
    it "sets aside and replenishes tokens without changing their identities" do
      let drawn = (newTokenBag [skull]) {bagCurrentToken = Just tablet}
      let aside = setAsideBagToken drawn
      bagSetAside aside `shouldBe` [tablet]
      bagCurrentToken aside `shouldBe` Nothing
      returnSetAsideTokens aside `shouldBe` newTokenBag [skull, tablet]
    it "returns a cancelled reveal without disturbing the set-aside pile" do
      let drawn = (newTokenBag [skull]) {bagCurrentToken = Just cultist, bagSetAside = [tablet]}
      returnBagToken drawn `shouldBe` (newTokenBag [skull, cultist]) {bagSetAside = [tablet]}

  describe "debug next draw" do
    it "forces an in-bag token once and follows the ordinary draw transition" do
      let bag = (newTokenBag [skull, tablet]) {bagDebugNext = Just Tablet}
      (drawn, remaining) <- drawBagToken bagTokenFace bag
      drawn `shouldBe` Just tablet
      bagTokens remaining `shouldBe` [skull]
      bagCurrentToken remaining `shouldBe` Just tablet
      bagDebugNext remaining `shouldBe` Nothing
    it "clears stale overrides without manufacturing tokens" do
      let bag = (newTokenBag [skull]) {bagDebugNext = Just Tablet}
      (drawn, remaining) <- drawBagToken bagTokenFace bag
      drawn `shouldBe` Just skull
      bagDebugNext remaining `shouldBe` Nothing
    it "rejects a request for a token that is only set aside" do
      let bag = (newTokenBag [skull]) {bagSetAside = [tablet]}
      debugTokenBag (toJSON Tablet) (toJSON bag) `shouldBe` Nothing
    it "accepts old story metadata and can clear an override" do
      updated <- expectJust $ debugTokenBag (toJSON Skull) (legacyBag "infestation")
      bag <- expectDecoded updated :: IO CustomChaosBag
      bagDebugNext bag `shouldBe` Just Skull
      cleared <- expectJust $ debugTokenBag Null updated
      clearedBag <- expectDecoded cleared :: IO CustomChaosBag
      bagDebugNext clearedBag `shouldBe` Nothing

  describe "debug contents editing" do
    let edit command bag = do
          updated <- editTokenBag command (toJSON (bag :: CustomChaosBag)) >>= expectJust
          expectDecoded updated :: IO CustomChaosBag
    it "adds a distinct physical token without replacing the other piles" do
      let bag = (newTokenBag [skull]) {bagSetAside = [tablet], bagCurrentToken = Just cultist}
      updated <- edit (object ["action" .= String "add", "face" .= Skull]) bag
      map bagTokenFace (bagTokens updated) `shouldBe` [Skull, Skull]
      length (bagTokens updated) `shouldBe` 2
      bagTokens updated `shouldNotBe` [skull, skull]
      bagSetAside updated `shouldBe` [tablet]
      bagCurrentToken updated `shouldBe` Just cultist
    it "removes only the selected token and clears an unavailable override" do
      let bag = (newTokenBag [skull, tablet]) {bagDebugNext = Just Skull}
      updated <- edit (object ["action" .= String "remove", "id" .= bagTokenId skull]) bag
      bagTokens updated `shouldBe` [tablet]
      bagDebugNext updated `shouldBe` Nothing
    it "moves tokens between piles without changing their IDs" do
      aside <-
        edit
          (object ["action" .= String "setAside", "id" .= bagTokenId skull])
          (newTokenBag [skull, tablet])
      bagTokens aside `shouldBe` [tablet]
      bagSetAside aside `shouldBe` [skull]
      returned <- edit (object ["action" .= String "return", "id" .= bagTokenId skull]) aside
      returned `shouldBe` newTokenBag [tablet, skull]
    it "returns all set-aside tokens but leaves the current reveal alone" do
      let bag = (newTokenBag [skull]) {bagSetAside = [tablet], bagCurrentToken = Just cultist}
      updated <- edit (object ["action" .= String "returnSetAside"]) bag
      bagTokens updated `shouldBe` [skull, tablet]
      bagSetAside updated `shouldBe` []
      bagCurrentToken updated `shouldBe` Just cultist
    it "rejects attempts to edit the current reveal or a stale token ID" do
      let bag = (newTokenBag [skull]) {bagCurrentToken = Just tablet}
      editTokenBag (object ["action" .= String "remove", "id" .= bagTokenId tablet]) (toJSON bag)
        `shouldReturn` Nothing
      editTokenBag (object ["action" .= String "setAside", "id" .= bagTokenId cultist]) (toJSON bag)
        `shouldReturn` Nothing
    it "keeps the existing face override protocol" do
      updated <- edit (toJSON Skull) (newTokenBag [skull])
      bagDebugNext updated `shouldBe` Just Skull

  describe "scenario-owned bags" do
    let attrs = scenario id "01104" "Test" Standard []
    it "defaults to no custom bags when loading an old scenario" do
      value <- expectObject $ toJSON attrs
      decoded <- expectDecoded (Object $ KeyMap.delete "customChaosBags" value) :: IO ScenarioAttrs
      scenarioCustomChaosBags decoded `shouldBe` Map.empty
    it "round-trips multiple independent named bags" do
      let bags = Map.fromList [("fury", newTokenBag [skull]), ("other", newTokenBag [tablet])]
      decoded <- expectDecoded $ toJSON attrs {scenarioCustomChaosBags = bags}
      scenarioCustomChaosBags decoded `shouldBe` bags

  describe "Fury" do
    it "draws two more tokens for a Moon without drawing any physical token twice" do
      let moon = cultist {bagTokenFace = MoonToken}
      let bag = (newTokenBag [skull, tablet, moon]) {bagDebugNext = Just MoonToken}
      (faces, drawn) <- drawFuryBagTokens bag 1
      faces `shouldSatisfy` \case
        MoonToken : rest -> sort rest == sort [Skull, Tablet]
        _ -> False
      bagTokens drawn `shouldBe` []
      sort (bagSetAside drawn) `shouldBe` sort [skull, tablet, moon]
      bagDebugNext drawn `shouldBe` Nothing
      sort (bagTokens $ returnSetAsideTokens drawn) `shouldBe` sort [skull, tablet, moon]
    it "terminates when Moon recursion exhausts the bag" do
      let moon = cultist {bagTokenFace = MoonToken}
      (faces, drawn) <- drawFuryBagTokens (newTokenBag [moon]) 1
      faces `shouldBe` [MoonToken]
      allBagTokens drawn `shouldBe` [moon]
