module Arkham.Campaigns.TheCircleUndone.CampaignSteps where

import Arkham.Prelude

import Arkham.Campaign.Types
import Arkham.CampaignStep

nextStep :: CampaignAttrs -> Maybe CampaignStep
nextStep a = case campaignStep a of
  Just PrologueStep -> Just DisappearanceAtTheTwilightEstate
  Just DisappearanceAtTheTwilightEstate -> Just TheWitchingHour
  Just TheWitchingHour -> Just (UpgradeDeckStep AtDeathsDoorstep)
  Just AtDeathsDoorstep -> Just (UpgradeDeckStep TheSecretName)
  Just (InterludeStep 2 _) -> Just TheSecretName
  Just TheSecretName -> Just (UpgradeDeckStep TheWagesOfSin)
  Just TheWagesOfSin -> Just (UpgradeDeckStep ForTheGreaterGood)
  Just ForTheGreaterGood -> Just (UpgradeDeckStep $ InterludeStep 3 Nothing)
  Just (InterludeStep 3 _) -> Just UnionAndDisillusion
  Just UnionAndDisillusion -> Just (UpgradeDeckStep InTheClutchesOfChaos)
  Just InTheClutchesOfChaos -> Just (UpgradeDeckStep $ InterludeStep 4  Nothing)
  Just (InterludeStep 4 _) -> Just BeforeTheBlackThrone
  Just BeforeTheBlackThrone -> Nothing
  Just EpilogueStep -> Nothing
  Just (UpgradeDeckStep nextStep') -> Just nextStep'
  _ -> Nothing

pattern DisappearanceAtTheTwilightEstate :: CampaignStep
pattern DisappearanceAtTheTwilightEstate <- ScenarioStep "05043" where
  DisappearanceAtTheTwilightEstate = ScenarioStep "05043"

pattern TheWitchingHour :: CampaignStep
pattern TheWitchingHour <- ScenarioStep "05050" where
  TheWitchingHour = ScenarioStep "05050"

pattern AtDeathsDoorstep :: CampaignStep
pattern AtDeathsDoorstep <- ScenarioStep "05065" where
  AtDeathsDoorstep = ScenarioStep "05065"

pattern TheSecretName :: CampaignStep
pattern TheSecretName <- ScenarioStep "05120" where
  TheSecretName = ScenarioStep "05120"

pattern TheWagesOfSin :: CampaignStep
pattern TheWagesOfSin <- ScenarioStep "05161" where
  TheWagesOfSin = ScenarioStep "05161"

pattern ForTheGreaterGood :: CampaignStep
pattern ForTheGreaterGood <- ScenarioStep "05197" where
  ForTheGreaterGood = ScenarioStep "05197"

pattern UnionAndDisillusion :: CampaignStep
pattern UnionAndDisillusion <- ScenarioStep "05238" where
  UnionAndDisillusion = ScenarioStep "05238"

pattern InTheClutchesOfChaos :: CampaignStep
pattern InTheClutchesOfChaos <- ScenarioStep "05284" where
  InTheClutchesOfChaos = ScenarioStep "05284"

pattern BeforeTheBlackThrone :: CampaignStep
pattern BeforeTheBlackThrone <- ScenarioStep "05325" where
  BeforeTheBlackThrone = ScenarioStep "05325"
