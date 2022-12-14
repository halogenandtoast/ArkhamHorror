module Arkham.Campaigns.TheForgottenAge.CampaignSteps where

import Arkham.Prelude

import Arkham.Campaign.Types
import Arkham.CampaignStep

nextStep :: CampaignAttrs -> Maybe CampaignStep
nextStep a = case campaignStep a of
  Just PrologueStep -> Just TheUntamedWilds
  Just TheUntamedWilds -> Just (InterludeStep 1 Nothing)
  Just (InterludeStep 1 _) -> Just (UpgradeDeckStep TheDoomOfEztli)
  Just TheDoomOfEztli -> Just (UpgradeDeckStep $ InterludeStep 2 Nothing)
  Just (InterludeStep 2 _) -> Just ThreadsOfFate
  Just ThreadsOfFate -> Just ResupplyPoint
  Just ResupplyPoint -> Just (UpgradeDeckStep TheBoundaryBeyond)
  Just TheBoundaryBeyond -> Just (UpgradeDeckStep $ InterludeStep 3 Nothing)
  Just (InterludeStep 3 _) -> Just HeartOfTheElders
  Just HeartOfTheElders -> Just (UpgradeDeckStep TheCityOfArchives)
  Just TheCityOfArchives -> Just (UpgradeDeckStep $ InterludeStep 4 Nothing)
  Just (InterludeStep 4 _) -> Just TheDepthsOfYoth
  Just TheDepthsOfYoth -> Just (UpgradeDeckStep $ InterludeStep 5 Nothing)
  Just (InterludeStep 5 _) -> Just ShatteredAeons
  Just ShatteredAeons -> Nothing
  Just EpilogueStep -> Just (UpgradeDeckStep TurnBackTime)
  Just TurnBackTime -> Nothing
  Just (UpgradeDeckStep nextStep') -> Just nextStep'
  _ -> Nothing

pattern TheUntamedWilds :: CampaignStep
pattern TheUntamedWilds <- ScenarioStep "04043" where
  TheUntamedWilds = ScenarioStep "04043"

pattern TheDoomOfEztli :: CampaignStep
pattern TheDoomOfEztli <- ScenarioStep "04054" where
  TheDoomOfEztli = ScenarioStep "04054"

pattern ThreadsOfFate :: CampaignStep
pattern ThreadsOfFate <- ScenarioStep "04113" where
  ThreadsOfFate = ScenarioStep "04113"

pattern TheBoundaryBeyond :: CampaignStep
pattern TheBoundaryBeyond <- ScenarioStep "04161" where
  TheBoundaryBeyond = ScenarioStep "04161"

pattern HeartOfTheElders :: CampaignStep
pattern HeartOfTheElders <- ScenarioStep "04205" where
  HeartOfTheElders = ScenarioStep "04205"

pattern TheCityOfArchives :: CampaignStep
pattern TheCityOfArchives <- ScenarioStep "04237" where
  TheCityOfArchives = ScenarioStep "04237"

pattern TheDepthsOfYoth :: CampaignStep
pattern TheDepthsOfYoth <- ScenarioStep "04277" where
  TheDepthsOfYoth = ScenarioStep "04277"

pattern ShatteredAeons :: CampaignStep
pattern ShatteredAeons <- ScenarioStep "04314" where
  ShatteredAeons = ScenarioStep "04314"

pattern TurnBackTime :: CampaignStep
pattern TurnBackTime <- ScenarioStep "04344" where
  TurnBackTime = ScenarioStep "04344"
