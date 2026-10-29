module Arkham.Homebrew.AgesUnwound.Campaign (agesUnwound) where

import Arkham.Campaign.Import.Lifted
import Arkham.Campaign.Types qualified as Campaign
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
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Xp
import Data.Map.Strict qualified as Map

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
    CampaignStep EpilogueStep -> scope "epilogue" do
      won <- getHasRecord TheInvestigatorsBoundAforgomonInAPrisonOfTime
      waning <- select $ IncludeEliminated $ investigatorWithRecord ExistenceIsWaning
      marked <-
        selectAny
          $ investigatorWithRecord StillBearsAforgomonsMark
          <> not_ KilledInvestigator
          <> not_ InsaneInvestigator
          <> not_ (investigatorWithRecord ExistenceIsWaning)
      silverTwilight <- getHasRecord YouHaveAdvancedTheSchemesOfTheSilverTwilightLodge
      flavor do
        setTitle "title"
        p.basic "instructions"
        compose.green.validate (won && notNull waning) do
          p.basic "waning.condition"
          p "waning.body"
          p.basic "waning.effect"
        compose.green.validate (won && marked) do
          p.basic "marked.condition"
          p "marked.body"
        compose.green.validate (won && silverTwilight) do
          p.basic "silverTwilight.condition"
          p "silverTwilight.body"
      when won $ for_ waning $ push . RemoveInvestigatorFromCampaign
      gameOverWon
      pure c
    -- Erasure in the epilogue forgets the campaign entry without the normal
    -- roster-change handler restarting the current campaign step.
    RemoveInvestigatorFromCampaign iid | (toAttrs c).normalizedStep == EpilogueStep -> do
      let forgetXp (Campaign.XpBreakdownStep step iids (XpBreakdown entries)) =
            Campaign.XpBreakdownStep step (filter (/= iid) iids) $ XpBreakdown $ filter keepEntry entries
          keepEntry = \case
            InvestigatorGainXp who _ -> who /= iid
            InvestigatorLoseXp who _ -> who /= iid
            TallyGained _ owner _ -> owner /= Just iid
            TallyLost _ owner _ -> owner /= Just iid
            AllGainXp _ -> True
      pure
        $ AgesUnwound
        $ toAttrs c
        & Campaign.decksL
        %~ Map.delete iid
        & Campaign.storyCardsL
        %~ Map.delete iid
        & Campaign.modifiersL
        %~ Map.delete iid
        & Campaign.xpBreakdownL
        %~ map forgetXp
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
