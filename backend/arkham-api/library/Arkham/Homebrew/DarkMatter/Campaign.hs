module Arkham.Homebrew.DarkMatter.Campaign (darkMatter) where

import Arkham.Campaign.Import.Lifted
import Arkham.Card (toCardDef)
import Arkham.ChaosToken
import Arkham.Helpers.Campaign (getCampaignStoryCards)
import Arkham.Helpers.FlavorText
import Arkham.Homebrew.DarkMatter.CampaignSteps
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.DarkMatter.Import
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Modifier (ModifierType (..))
import Arkham.Source

newtype DarkMatter = DarkMatter CampaignAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor)

darkMatter :: Difficulty -> DarkMatter
darkMatter = campaign DarkMatter (CampaignId ":dark-matter") "Dark Matter"

instance IsCampaign DarkMatter where
  campaignTokens = chaosBagContents
  nextStep a = case (toAttrs a).normalizedStep of
    PrologueStep -> continue TheTatterdemalion
    TheTatterdemalion -> continue ElectricNightmare
    -- Electric Nightmare's loss resolution ends the campaign from the
    -- scenario itself; otherwise Interlude I follows.
    ElectricNightmare -> continue MissionBriefing
    -- After each Scenario III, return to The Search for Fragment (guide p10)
    -- until all three traces are exhausted; that interlude picks the next
    -- destination (or Introspection) itself.
    LostQuantum -> continue TheSearchForFragment
    InTheShadowOfEarth -> continue TheSearchForFragment
    StrangeMoons -> continue TheSearchForFragment
    TheMachineInYellow -> continue FragmentOfCarcosa
    FragmentOfCarcosa -> continue Starfall
    Starfall -> continue EpilogueStep
    EpilogueStep -> Nothing
    other -> defaultNextStep other

instance RunMessage DarkMatter where
  runMessage msg c = runQueueT $ campaignI18n $ case msg of
    CampaignStep PrologueStep -> do
      scope "intro" $ flavor $ setTitle "title" >> p "body"
      scope "additionalRulesAndClarifications" do
        flavor $ setTitle "title" >> p "scan"
        flavor $ setTitle "title" >> p "memories"
        flavor $ setTitle "title" >> p "alert"
      scope "prologue" $ flavor $ setTitle "title" >> p "body"
      nextCampaignStep
      pure c
    -- Interlude I: Mission Briefing
    CampaignStep (InterludeStep 1 _) -> scope "missionBriefing" do
      transportedByMaja <- getHasRecord YouWereTransportedToTheVirtualDreamlandsByMaja
      flavor do
        setTitle "title"
        p $ if transportedByMaja then "missionBriefing1" else "missionBriefing2"
      flavor $ setTitle "title" >> p "missionBriefing3"
      storyWithChooseOneM' (setTitle "title" >> p "simulatedPerformance") do
        labeled' "watchThePerformance" $ doStep 1 msg
        labeled' "declineToWatch" nothing
      doStep 2 msg
      pure c
    -- Heir to Carcosa (read at your own risk)
    DoStep 1 (CampaignStep (InterludeStep 1 _)) -> scope "heirToCarcosa" do
      flavor $ setTitle "title" >> p "body"
      record YouHaveWatchedThePerformanceOfHeirToCarcosa
      eachInvestigator \iid -> sufferMentalTrauma iid 1
      storyCards <- getCampaignStoryCards
      for_ (mapToList storyCards) \(iid, cards) ->
        when (any ((== Assets.heirToCarcosa) . toCardDef) cards) do
          chooseOneM iid do
            labeled' "addTwoMemories" $ addMemories iid 2
            labeled' "doNotAddMemories" nothing
      pure c
    DoStep 2 (CampaignStep (InterludeStep 1 _)) -> theSearchForFragment c
    -- The Search for Fragment, revisited after each Scenario III
    CampaignStep (InterludeStep 2 _) -> theSearchForFragment c
    -- Interlude II: Introspection
    CampaignStep (InterludeStep 3 _) -> scope "introspection" do
      flavor $ setTitle "title" >> p "body"
      storyWithChooseOneM' (setTitle "title" >> p "searchTheTatterdemalion") do
        labeled' "searchTheShip" do
          addImpendingDoom 1
          eachInvestigator \iid -> addMemories iid 1
        labeled' "doNotSearchTheShip" nothing
      -- The side-story option (crossing out Memories instead of paying
      -- experience) is resolved manually; surface the guide text so players
      -- know it exists.
      flavor $ setTitle "title" >> p "sideStory"
      let difficulty = (toAttrs c).difficulty
      addChaosToken $ case difficulty of
        Easy -> MinusThree
        Standard -> MinusFive
        Hard -> MinusSix
        Expert -> MinusSeven
      when (difficulty `elem` [Hard, Expert]) $ addChaosToken Cultist
      whenHasRecord YouHaveUncoveredTheCultistsInhumanMethods $ doStep 1 msg
      setNextCampaignStep TheMachineInYellow
      pure c
    -- Introspection 1
    DoStep 1 (CampaignStep (InterludeStep 3 _)) -> scope "introspection" do
      flavor $ setTitle "title" >> p "introspection1"
      eachInvestigator \iid -> do
        scenarioSetupModifier ":dark-matter:193" CampaignSource iid (StartingResources 1)
        scenarioSetupModifier ":dark-matter:193" CampaignSource iid (StartingHand 1)
      pure c
    CampaignStep EpilogueStep -> scope "epilogue" do
      hopeShielded <- getHasRecord HopeWasShieldedFromTheBlast
      uccEscaped <- getHasRecord TheUCCEscapedToAnotherGalaxy
      miGoReturned <- getHasRecord MiGoSafelyReturnedToTheirHomeWorld
      if hopeShielded || uccEscaped
        then do
          flavor $ setTitle "title" >> p "epilogue1"
          if miGoReturned
            then do
              flavor $ setTitle "title" >> p "epilogue4"
              record HasturAndTassildaAreImprisonedInCarcosaOnceMore
            else do
              flavor $ setTitle "title" >> p "epilogue3"
              record TheRealmOfCarcosaOvertookOurUniverse
        else do
          flavor $ setTitle "title" >> p "epilogue2"
          flavor $ setTitle "title" >> p "epilogue3"
          record TheRealmOfCarcosaOvertookOurUniverse
      gameOver
      pure c
    _ -> lift $ defaultCampaignRunner msg c

{- | The Search for Fragment (guide p10): choose a trace that has not already
been chosen; when every Scenario III is done, skip to Introspection.
-}
theSearchForFragment :: (HasI18n, ReverseQueue m) => DarkMatter -> m DarkMatter
theSearchForFragment c = scope "theSearchForFragment" do
  let completed = (toAttrs c).completedSteps
  let remaining = filter (`notElem` completed) [LostQuantum, InTheShadowOfEarth, StrangeMoons]
  if null remaining
    then setNextCampaignStep Introspection
    else storyWithChooseOneM'
      ( do
          setTitle "title"
          p "body"
          when (LostQuantum `elem` remaining) $ p "trace1"
          when (InTheShadowOfEarth `elem` remaining) $ p "trace2"
          when (StrangeMoons `elem` remaining) $ p "trace3"
      )
      do
        labeledValidate' (LostQuantum `elem` remaining) "lostQuantum"
          $ setNextCampaignStep LostQuantum
        labeledValidate' (InTheShadowOfEarth `elem` remaining) "inTheShadowOfEarth"
          $ setNextCampaignStep InTheShadowOfEarth
        labeledValidate' (StrangeMoons `elem` remaining) "strangeMoons"
          $ setNextCampaignStep StrangeMoons
  pure c
