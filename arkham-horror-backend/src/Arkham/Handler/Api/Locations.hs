module Arkham.Handler.Api.Locations where

import Import
import Arkham.Types
import qualified Data.List.NonEmpty as NE
import System.Random
import Prelude ((!!))

sample :: [a] -> IO a
sample l = (toList l !!) <$> randomRIO (0, length l - 1)

getRandomToken :: NE.NonEmpty ArkhamChaosToken -> IO ArkhamChaosToken
getRandomToken = sample . toList

tokenPool :: NE.NonEmpty ArkhamChaosToken
tokenPool = NE.fromList [ArkhamChaosTokenNumber (-1)]

tokenToModifier :: ArkhamChaosToken -> Int
tokenToModifier _ = -1

investigatorSkillValue :: ArkhamSkill -> ArkhamInvestigator -> Int
investigatorSkillValue ArkhamSkillWillpower = arkhamInvestigatorWillpower
investigatorSkillValue ArkhamSkillIntellect = arkhamInvestigatorIntellect
investigatorSkillValue ArkhamSkillCombat = arkhamInvestigatorCombat
investigatorSkillValue ArkhamSkillAgility = arkhamInvestigatorAgility

shroudValue :: ArkhamLocation -> Maybe Int
shroudValue (ArkhamLocationUnrevealed _) = Nothing
shroudValue (ArkhamLocationRevealed l) = pure $ arkhamLocationRevealedDataShroud l

rolandBanks :: ArkhamInvestigator
rolandBanks = ArkhamInvestigator
  { arkhamInvestigatorName = "Roland Banks"
  , arkhamInvestigatorWillpower = 3
  , arkhamInvestigatorIntellect = 3
  , arkhamInvestigatorCombat = 4
  , arkhamInvestigatorAgility = 2
  , arkhamInvestigatorHealth = 9
  , arkhamInvestigatorSanity = 5
  , arkhamInvestigatorFrontImageUrl = "https://arkhamdb.com/bundles/cards/01001.png"
  , arkhamInvestigatorBackImageUrl = "https://arkhamdb.com/bundles/cards/01001b.png"
  }

study :: ArkhamLocation
study = ArkhamLocationRevealed $
  ArkhamLocationRevealedData
    { arkhamLocationRevealedDataId = "1"
    , arkhamLocationRevealedDataName = "Study"
    , arkhamLocationRevealedDataSymbol = ArkhamLocationSymbolCircle
    , arkhamLocationRevealedDataConnections = []
    , arkhamLocationRevealedDataShroud = 2
    , arkhamLocationRevealedDataMaxClues = ArkhamClueCountPerInvestigator 2
    , arkhamLocationRevealedDataCurrentClues = 2
    , arkhamLocationRevealedDataImageUrl = "https://arkhamdb.com/bundles/cards/01111.png"
    }

postApiV1ArkhamGameLocationsRevealR :: ArkhamHorrorGameId -> Handler ArkhamLocation
postApiV1ArkhamGameLocationsRevealR _ = pure study

postApiV1ArkhamGameLocationsInvestigateR :: ArkhamHorrorGameId -> Handler ArkhamSkillTestResult
postApiV1ArkhamGameLocationsInvestigateR _ =
  case shroudValue study of
    Nothing -> error "Could not investgate"
    Just difficulty -> do
      token <- liftIO $ getRandomToken tokenPool
      let skillValue = investigatorSkillValue ArkhamSkillIntellect rolandBanks
      -- TODO: commit skill cards
      let finalValue = skillValue - difficulty - tokenToModifier token
      let result = if finalValue >= 0 then ArkhamSkillTestResultTypeSuccess else ArkhamSkillTestResultTypeFailure
      pure ArkhamSkillTestResult
        { arkhamSkillTestResultToken = token
        , arkhamSkillTestResultBase = skillValue
        , arkhamSkillTestResultSkill = ArkhamSkillIntellect
        , arkhamSkillTestResultAction = ArkhamActionInvestigate 0
        , arkhamSkillTestResultType = result
        }
