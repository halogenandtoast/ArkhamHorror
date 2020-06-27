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
findCard cardCode' = do
  cardList' <- cardList
  case cardList' of
    Left err -> throwString err
    Right cards -> pure $ toArkhamCard <$> find ((== cardCode') . code) cards

toArkhamCard :: ArkhamDbCard -> ArkhamCard
toArkhamCard ArkhamDbCard {..} = PlayerCard $ ArkhamPlayerCard
  name
  cost
  (ArkhamCardCode code)
  ("https://arkhamdb.com/"
  <> fromMaybe ("/bundles/cards/" <> code <> ".png") imagesrc
  )
  (Just 0)

loadGameFixture :: Int -> IO ArkhamGameData
loadGameFixture 1 =
  ArkhamGameData 1 NightOfTheZealot theGathering ArkhamEasy
    . fixtureGameState 1
    <$> loadDeck "20344"
loadGameFixture 2 =
  ArkhamGameData 1 NightOfTheZealot theGathering ArkhamEasy
    . fixtureGameState 2
    <$> loadDeck "101"
loadGameFixture _ = throwString "Unknown fixture"

theGathering :: ArkhamScenario
theGathering = ArkhamScenario
  (ArkhamScenarioCode "theGathering")
  "The Gathering"
  "https://arkhamdb.com/bundles/cards/01104.jpg"

fixtureGameState :: Int -> [ArkhamCard] -> ArkhamGameState
fixtureGameState seed deck' = ArkhamGameState
  (playerF seed deck')
  Investigation
  chaosTokens
  (HashMap.fromList $ map (\l -> (alCardCode l, l)) [study seed])
  [agenda, act]
  ArkhamGameStateStepInvestigatorActionStep

loadDeck :: String -> IO [ArkhamCard]
loadDeck deckId = do
  deckList <- eitherDecode
    <$> simpleHttp ("https://arkhamdb.com/api/public/decklist/" <> deckId)
  case deckList of
    Left err -> throwString err
    Right cards -> shuffleM =<< HashMap.foldMapWithKey
      (\cardId count -> do
        mcard <- findCard cardId
        pure $ maybe [] (replicate count) mcard
      )
      (slots cards)

agenda :: ArkhamStack
agenda = AgendaStack $ ArkhamAgenda
  (ArkhamCardCode "01105")
  "https://arkhamdb.com/bundles/cards/01105.jpg"
  0

act :: ArkhamStack
act = ActStack $ ArkhamAct
  (ArkhamCardCode "01108")
  "https://arkhamdb.com/bundles/cards/01108.jpg"

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

study :: Int -> ArkhamLocation
study seed = ArkhamLocation
  "Study"
  (ArkhamCardCode "01111")
  []
  2
  "https://arkhamdb.com/bundles/cards/01111.png"
  [investigatorF seed]
  0
  0
  Revealed

playerF :: Int -> [ArkhamCard] -> ArkhamPlayer
playerF seed deck' = ArkhamPlayer (investigatorF seed) 0 0 5 0 [] [] deck' []

investigatorF :: Int -> ArkhamInvestigator
investigatorF 2 = daisyWalker
investigatorF _ = rolandBanks


rolandBanks :: ArkhamInvestigator
rolandBanks = ArkhamInvestigator
  "Roland Banks"
  "https://arkhamdb.com/bundles/cards/01001.png"
  "/img/arkham/AHC01_1_portrait.jpg"
  (ArkhamSkill 3)
  (ArkhamSkill 3)
  (ArkhamSkill 4)
  (ArkhamSkill 2)
  (ArkhamCardCode "01001")

daisyWalker :: ArkhamInvestigator
daisyWalker = ArkhamInvestigator
  "Daisy Walker"
  "https://arkhamdb.com/bundles/cards/01002.png"
  "/img/arkham/AHC01_2_portrait.jpg"
  (ArkhamSkill 3)
  (ArkhamSkill 5)
  (ArkhamSkill 2)
  (ArkhamSkill 2)
  (ArkhamCardCode "01002")
