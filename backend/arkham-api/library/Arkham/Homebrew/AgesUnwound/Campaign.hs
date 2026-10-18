module Arkham.Homebrew.AgesUnwound.Campaign (agesUnwound) where

import Arkham.Campaign.Import.Lifted
import Arkham.CampaignLog (CampaignLog (..))
import Arkham.CampaignLogKey (toCampaignLogKey)
import Arkham.Card (toCard)
import Arkham.ChaosToken
import Arkham.Deck qualified as Deck
import Arkham.Helpers.FlavorText
import Arkham.Homebrew.AgesUnwound.CampaignSteps
import Arkham.Homebrew.AgesUnwound.DummyDeck (isDummyDeckCard)
import Arkham.Homebrew.AgesUnwound.Import
import Arkham.Homebrew.AgesUnwound.ScenarioDeckKeys (pattern DummyDeck)
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
        flavor $ setTitle "title" >> compose (scope "alert" $ h3 "title" >> p "body")
        flavor $ setTitle "title" >> compose (scope "storyCards" $ h3 "title" >> p "body")
        flavor $ setTitle "title" >> compose (scope "swarming" $ h3 "title" >> p "body")
        flavor $ setTitle "title" >> compose (scope "soloPlay" $ h3 "title" >> p "body")
      scope "prologue" $ flavor $ setTitle "title" >> p "body"
      -- "Mark 1 Strange Assistance in your Campaign Log."
      incrementRecordCount StrangeAssistance 1
      nextCampaignStep
      pure c
    -- Interlude I: An Unknown Benefactor
    CampaignStep (InterludeStep 1 _) -> scope "anUnknownBenefactor" do
      storyWithChooseOneM (setTitle "title" >> p "body") do
        labeled "goingItAlone" do
          flavor $ setTitle "title" >> p "goingItAlone"
          record TheInvestigatorsDeclinedAnOfferOfHelp
          removeAllChaosTokens Cultist
          addChaosToken Tablet
        labeled "leapOfFaith" do
          flavor $ setTitle "title" >> p "leapOfFaith"
          record TheInvestigatorsAcceptedAnOfferOfHelp
          incrementRecordCount StrangeAssistance 1
          removeAllChaosTokens Tablet
          addChaosToken Cultist
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
      gameOverWon
      pure c
    {- "When a card from the dummy deck would leave play, instead shuffle it back
    into the dummy deck." Every player card that leaves play funnels through
    'AddToDiscard', and the campaign entity is handed in-scenario messages too,
    so one case here covers every scenario that builds the deck. The discard has
    already happened by the time this runs: 'shuffleCardsIntoDeck' obtains the
    card first, which takes it back out again. -}
    AddToDiscard _ pc | isDummyDeckCard (toCard pc) -> do
      shuffleCardsIntoDeck (Deck.ScenarioDeckByKey DummyDeck) [toCard pc]
      pure c
    _ -> lift $ defaultCampaignRunner msg c
