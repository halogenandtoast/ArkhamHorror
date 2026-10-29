-- The two chapter II scenarios are played in partner-dependent order;
-- all three chapter IV destinations are visited in the chosen order.
module Arkham.Homebrew.NightOfVespers.CampaignSteps where

import Arkham.CampaignStep
import Arkham.Prelude

pattern TheEndOfSeasons :: CampaignStep
pattern TheEndOfSeasons <- ScenarioStep ":night-of-vespers:001"
  where
    TheEndOfSeasons = ScenarioStep ":night-of-vespers:001"

pattern BehindClosedDoors :: CampaignStep
pattern BehindClosedDoors <- ScenarioStep ":night-of-vespers:037"
  where
    BehindClosedDoors = ScenarioStep ":night-of-vespers:037"

pattern DancingWithDeath :: CampaignStep
pattern DancingWithDeath <- ScenarioStep ":night-of-vespers:075"
  where
    DancingWithDeath = ScenarioStep ":night-of-vespers:075"

pattern TheTeachingsOfAradia :: CampaignStep
pattern TheTeachingsOfAradia <- ScenarioStep ":night-of-vespers:105"
  where
    TheTeachingsOfAradia = ScenarioStep ":night-of-vespers:105"

pattern TheCityOfFog :: CampaignStep
pattern TheCityOfFog <- ScenarioStep ":night-of-vespers:136"
  where
    TheCityOfFog = ScenarioStep ":night-of-vespers:136"

pattern TheGardenOfBones :: CampaignStep
pattern TheGardenOfBones <- ScenarioStep ":night-of-vespers:168"
  where
    TheGardenOfBones = ScenarioStep ":night-of-vespers:168"

pattern AChorusOfSuffering :: CampaignStep
pattern AChorusOfSuffering <- ScenarioStep ":night-of-vespers:214"
  where
    AChorusOfSuffering = ScenarioStep ":night-of-vespers:214"

pattern MothersMercy :: CampaignStep
pattern MothersMercy <- ScenarioStep ":night-of-vespers:257"
  where
    MothersMercy = ScenarioStep ":night-of-vespers:257"

pattern TwoMessages :: CampaignStep
pattern TwoMessages = InterludeStep 1 Nothing

pattern TheRoseOfTuscany :: CampaignStep
pattern TheRoseOfTuscany = InterludeStep 2 Nothing

pattern TheThreeMothers :: CampaignStep
pattern TheThreeMothers = InterludeStep 3 Nothing

pattern OurLadiesOfSorrow :: CampaignStep
pattern OurLadiesOfSorrow = InterludeStep 4 Nothing

-- Travel after a chapter IV scenario; distinct from the initial research interlude.
pattern ChooseDestination :: CampaignStep
pattern ChooseDestination = CampaignSpecificStep "travel" Nothing
