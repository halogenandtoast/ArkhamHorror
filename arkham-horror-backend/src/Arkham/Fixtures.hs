module Arkham.Fixtures where

import Arkham.Types
import ClassyPrelude
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HashMap
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Network.HTTP.Conduit (simpleHttp)
import System.Random.Shuffle

newtype ArkhamDbDeckList = ArkhamDbDeckList { slots :: HashMap Text Int }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data ArkhamDbCard = ArkhamDbCard
  { code :: Text
  , name :: Text
  , cost :: Maybe Int
  , imagesrc :: Maybe Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

cardList :: IO (Either String [ArkhamDbCard])
cardList = eitherDecode <$> BSL.readFile "data/cards.json"

findCard :: Text -> IO (Maybe ArkhamCard)
findCard cardCode = do
  cardList' <- cardList
  case cardList' of
    Left err -> throwString err
    Right cards -> pure $ toArkhamCard <$> find ((== cardCode) . code) cards

toArkhamCard :: ArkhamDbCard -> ArkhamCard
toArkhamCard ArkhamDbCard {..} =
  PlayerCard
    $ ArkhamPlayerCard name cost
    $ "https://arkhamdb.com/"
    <> fromMaybe ("/bundles/cards/" <> code <> ".png") imagesrc

loadGameFixture :: Int -> IO ArkhamGameData
loadGameFixture _ =
  ArkhamGameData 1 NightOfTheZealot theGathering . fixtureGameState <$> loadDeck

theGathering :: ArkhamScenario
theGathering =
  ArkhamScenario "The Gathering" "https://arkhamdb.com/bundles/cards/01104.jpg"

fixtureGameState :: [ArkhamCard] -> ArkhamGameState
fixtureGameState deck' = ArkhamGameState
  (playerF deck')
  Investigation
  chaosTokens
  (HashMap.fromList [("Study", RevealedLocation study)])
  [agenda, act]
  ArkhamGameStateStepInvestigatorActionStep

loadDeck :: IO [ArkhamCard]
loadDeck = do
  deckList <- eitherDecode
    <$> simpleHttp "https://arkhamdb.com/api/public/decklist/20344"
  case deckList of
    Left err -> throwString err
    Right cards ->
      shuffleM
        =<< HashMap.foldMapWithKey
                (\cardId count -> do
                  mcard <- findCard cardId
                  pure $ maybe [] (replicate count) mcard
                ) (slots cards)

agenda :: ArkhamStack
agenda =
  AgendaStack $ ArkhamAgenda "https://arkhamdb.com/bundles/cards/01105.jpg"

act :: ArkhamStack
act = ActStack $ ArkhamAct "https://arkhamdb.com/bundles/cards/01108.jpg"

chaosTokens :: NonEmpty ArkhamChaosToken
chaosTokens = NE.fromList
  [ PlusOne
  , PlusOne
  , Zero
  , Zero
  , Zero
  , MinusOne
  , MinusOne
  , MinusOne
  , MinusTwo
  , MinusTwo
  , Skull
  , Skull
  , Cultist
  , Tablet
  , AutoFail
  , ElderSign
  ]

study :: ArkhamRevealedLocation
study = ArkhamRevealedLocation
  "Study"
  "Study"
  []
  2
  "https://arkhamdb.com/bundles/cards/01111.png"
  [LocationInvestigator rolandBanks, LocationClues 2]

playerF :: [ArkhamCard] -> ArkhamPlayer
playerF deck' = ArkhamPlayer rolandBanks 0 0 5 0 [] [] deck'

rolandBanks :: ArkhamInvestigator
rolandBanks = ArkhamInvestigator
  "Roland Banks"
  "https://arkhamdb.com/bundles/cards/01001.png"
  "/img/arkham/AHC01_1_portrait.jpg"
  (ArkhamSkill 3)
  (ArkhamSkill 3)
  (ArkhamSkill 4)
  (ArkhamSkill 2)
