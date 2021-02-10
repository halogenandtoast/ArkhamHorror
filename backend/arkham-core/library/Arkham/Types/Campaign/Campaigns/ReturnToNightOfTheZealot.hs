module Arkham.Types.Campaign.Campaigns.ReturnToNightOfTheZealot where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.Effect.Window
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.EncounterSet (EncounterSet)
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.Exception
import Arkham.Types.GameValue
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.ScenarioId
import Arkham.Types.SkillId
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Stats (Stats)
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.TreacheryId
import Arkham.Types.Window
 hiding (Cultist)

import Arkham.Types.Campaign.Attrs
import Arkham.Types.Campaign.Campaigns.NightOfTheZealot
import Arkham.Types.Campaign.Runner
import Arkham.Types.CampaignStep
import Arkham.Types.Difficulty

newtype ReturnToNightOfTheZealot = ReturnToNightOfTheZealot NightOfTheZealot
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

returnToNightOfTheZealot :: Difficulty -> ReturnToNightOfTheZealot
returnToNightOfTheZealot difficulty =
  ReturnToNightOfTheZealot . NightOfTheZealot $ baseAttrs
    (CampaignId "50")
    "Return to the Night of the Zealot"
    difficulty
    (nightOfTheZealotChaosBagContents difficulty)

instance (CampaignRunner env) => RunMessage env ReturnToNightOfTheZealot where
  runMessage msg (ReturnToNightOfTheZealot nightOfTheZealot'@(NightOfTheZealot attrs@CampaignAttrs {..}))
    = case msg of
      NextCampaignStep _ -> do
        let
          nextStep = case campaignStep of
            Just PrologueStep -> Just (ScenarioStep "50011")
            Just (ScenarioStep "50011") -> Just (ScenarioStep "50025")
            Just (ScenarioStep "50025") -> Just (ScenarioStep "50032")
            _ -> Nothing
        unshiftMessage (CampaignStep nextStep)
        pure
          . ReturnToNightOfTheZealot
          . NightOfTheZealot
          $ attrs
          & (stepL .~ nextStep)
          & (completedStepsL %~ completeStep campaignStep)
      _ -> ReturnToNightOfTheZealot <$> runMessage msg nightOfTheZealot'
