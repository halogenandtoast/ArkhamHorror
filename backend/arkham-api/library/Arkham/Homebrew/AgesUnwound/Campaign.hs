module Arkham.Homebrew.AgesUnwound.Campaign (agesUnwound) where

import Arkham.Campaign.Import.Lifted
import Arkham.CampaignLog (CampaignLog (..))
import Arkham.CampaignLogKey (toCampaignLogKey)
import Arkham.Helpers.FlavorText
import Arkham.Homebrew.AgesUnwound.CampaignSteps
import Arkham.Homebrew.AgesUnwound.Import
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log

newtype AgesUnwound = AgesUnwound CampaignAttrs
  deriving anyclass HasModifiersFor
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

agesUnwound :: Difficulty -> AgesUnwound
agesUnwound = campaign AgesUnwound (CampaignId ":ages-unwound") "Ages Unwound"

{- | Scenarios III and VII can send the table back to themselves. The back-edge
is a 'continueNoUpgrade' (no XP spend on a retry) guarded by an uncrossed
record: the resolution records the key, and the scenario's own Setup crosses it
out, so the loop runs at most once per record.
-}
mustReplay :: CampaignAttrs -> AgesUnwoundKey -> Bool
mustReplay attrs k =
  let key = toCampaignLogKey k
   in key `member` attrs.log.recorded && key `notMember` campaignLogCrossedOut attrs.log

instance IsCampaign AgesUnwound where
  campaignTokens = chaosBagContents

  -- `normalizedStep`, not a bare `ScenarioStep` pattern: picking a lead makes
  -- the step a `ScenarioStepWithOptions`, which a bare pattern misses, and the
  -- campaign then hits GameOver after every scenario.
  nextStep a = case attrs.normalizedStep of
    PrologueStep -> continue NightOfFire
    NightOfFire -> continue AnUnknownBenefactor
    AnUnknownBenefactor -> continue TheMyriadGentleman
    TheMyriadGentleman -> continue AWorldTornDown
    AWorldTornDown
      | mustReplay attrs MustReplayAWorldTornDown -> continueNoUpgrade AWorldTornDown
      | otherwise -> continue Unstuck
    Unstuck -> continue AYearToPlan
    AYearToPlan -> continue AWorldTornDownAgain
    AWorldTornDownAgain -> continue TimeRunsOut
    TimeRunsOut
      | mustReplay attrs MustReplayTimeRunsOut -> continueNoUpgrade TimeRunsOut
      | otherwise -> continue EpilogueStep
    EpilogueStep -> Nothing
    other -> defaultNextStep other
   where
    attrs = toAttrs a

instance RunMessage AgesUnwound where
  runMessage msg c = runQueueT $ campaignI18n $ case msg of
    CampaignStep PrologueStep -> do
      scope "intro" $ flavor $ setTitle "title" >> p "body"
      scope "additionalRulesAndClarifications" do
        flavor $ setTitle "title" >> p "alert"
        flavor $ setTitle "title" >> p "storyCards"
        flavor $ setTitle "title" >> p "gainingAndLosingActions"
      scope "prologue" $ flavor $ setTitle "title" >> p "body"
      -- "Mark 1 Strange Assistance in your Campaign Log."
      incrementRecordCount StrangeAssistance 1
      nextCampaignStep
      pure c
    -- Interlude I: An Unknown Benefactor
    CampaignStep (InterludeStep 1 _) -> scope "anUnknownBenefactor" do
      flavor $ setTitle "title" >> p "body"
      storyWithChooseOneM (setTitle "title" >> p "theChoice") do
        labeled "goingItAlone" do
          flavor $ setTitle "title" >> p "goingItAlone"
          record TheInvestigatorsDeclinedAnOfferOfHelp
        labeled "leapOfFaith" do
          flavor $ setTitle "title" >> p "leapOfFaith"
          record TheInvestigatorsAcceptedAnOfferOfHelp
          incrementRecordCount StrangeAssistance 1
      nextCampaignStep
      pure c
    {- TODO(ages-unwound): the epilogue's full text. It branches three ways on
    what Scenario VII recorded -- an investigator whose existence is waning, an
    investigator who still bears Aforgomon's mark, and whether the Silver
    Twilight Lodge's schemes were advanced (a key written by a *different*
    campaign's log). The branches below are the real shape; only the flavour
    is missing. -}
    CampaignStep EpilogueStep -> scope "epilogue" do
      bound <- getHasRecord TheInvestigatorsBoundAforgomonInAPrisonOfTime
      silverTwilight <- getHasRecord YouHaveAdvancedTheSchemesOfTheSilverTwilightLodge
      flavor $ setTitle "title" >> p (if bound then "bound" else "allTimesAreOne")
      when silverTwilight $ flavor $ setTitle "title" >> p "silverTwilight"
      gameOver
      pure c
    _ -> lift $ defaultCampaignRunner msg c
