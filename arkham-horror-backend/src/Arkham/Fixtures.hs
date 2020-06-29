module Arkham.Fixtures where

import Arkham.Entity.ArkhamGame
import Arkham.Internal.Scenario
import Arkham.Internal.Types
import Arkham.Types
import Arkham.Types.Card
import Arkham.Types.ChaosToken
import Arkham.Types.Difficulty
import Arkham.Types.Game
import Arkham.Types.GameState
import Arkham.Types.Investigator
import Arkham.Types.Player
import Arkham.Types.Scenario
import Arkham.Types.Skill
import ClassyPrelude
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HashMap
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Lens.Micro
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
  , text :: Maybe Text
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
  (maybe False ("Fast." `isInfixOf`) text)

loadGameFixture :: Int -> IO ArkhamGame
loadGameFixture n = do
  fixture <- ArkhamGame <$> loadGameDataFixture n
  let setup = scenarioSetup (toInternalScenario fixture)
  traverseOf (currentData . gameState) setup fixture

loadGameDataFixture :: Int -> IO ArkhamGameData
loadGameDataFixture 1 =
  ArkhamGameData 1 NightOfTheZealot theGatheringF ArkhamEasy
    . fixtureGameState 1
    <$> loadDeck "20344"
loadGameDataFixture 2 =
  ArkhamGameData 1 NightOfTheZealot theGatheringF ArkhamEasy
    . fixtureGameState 2
    <$> loadDeck "101"
loadGameDataFixture _ = throwString "Unknown fixture"

theGatheringF :: ArkhamScenario
theGatheringF = ArkhamScenario
  (ArkhamScenarioCode "theGathering")
  "The Gathering"
  "https://arkhamdb.com/bundles/cards/01104.jpg"

fixtureGameState :: Int -> [ArkhamCard] -> ArkhamGameState
fixtureGameState seed deck' = ArkhamGameState
  (playerF seed deck')
  Investigation
  chaosTokens
  mempty
  mempty
  -- (HashMap.fromList $ map (\l -> (alCardCode l, l)) [study seed])
  -- (HashMap.fromList [("Agenda", agenda), ("Act", act)])
  ArkhamGameStateStepInvestigatorActionStep
  (Just InvestigationTakeActions)

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

playerF :: Int -> [ArkhamCard] -> ArkhamPlayer
playerF seed deck' = ArkhamPlayer
  (investigatorF seed)
  0
  0
  5
  0
  hand'
  []
  deck''
  []
  3
  False
  where (hand', deck'') = splitAt 5 deck'

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
