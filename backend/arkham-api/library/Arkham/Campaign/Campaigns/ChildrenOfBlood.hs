module Arkham.Campaign.Campaigns.ChildrenOfBlood (childrenOfBlood) where

import Arkham.Asset.Cards.ChildrenOfBlood qualified as Assets
import Arkham.Campaign.Import.Lifted
import Arkham.Campaigns.ChildrenOfBlood.CampaignSteps
import Arkham.Campaigns.ChildrenOfBlood.ChaosBag
import Arkham.Campaigns.ChildrenOfBlood.Helpers
import Arkham.Campaigns.ChildrenOfBlood.Key
import Arkham.Card
import Arkham.Helpers.Campaign (getCampaignStoryCards)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Log (getHasRecord)
import Arkham.Helpers.Query (allInvestigators)
import Arkham.Helpers.SkillTest (getSkillTestRevealedChaosTokens)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Source
import Arkham.Target (pattern Initiator)
import Arkham.Treachery.CardDefs.ChildrenOfBlood.Infected qualified as Treacheries

newtype ChildrenOfBlood = ChildrenOfBlood CampaignAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor)

childrenOfBlood :: Difficulty -> ChildrenOfBlood
childrenOfBlood = campaign ChildrenOfBlood (CampaignId "13") "Children of blood"

instance IsCampaign ChildrenOfBlood where
  campaignTokens = chaosBagContents
  nextStep a = case campaignStep (toAttrs a) of
    PrologueStep -> continue RiverOfBlood
    RiverOfBlood -> continue NewHorizons
    NewHorizons -> continue $ InterludeStep 1 Nothing
    InterludeStep 1 _ -> continue BloodMoney
    BloodMoney -> continue EpilogueStep
    other -> defaultNextStep other

instance RunMessage ChildrenOfBlood where
  runMessage msg c@(ChildrenOfBlood _attrs) = runQueueT $ campaignI18n $ case msg of
    CampaignStep PrologueStep -> do
      scope "intro" $ flavor $ setTitle "title" >> p "body"
      scope "additionalRulesAndClarifications" do
        flavor $ setTitle "title" >> p "bloodTokens"
        flavor $ setTitle "title" >> p "predator"
      scope "prologue" $ flavor $ setTitle "title" >> p "body"
      nextCampaignStep
      pure c
    CampaignStep (InterludeStep 1 _) -> scope "friendsInLowPlaces" do
      bloods <- selectCount (chaosToken_ #blood)
      if bloods == 12
        then do
          flavor $ setTitle "title" >> p "friendsInLowPlaces1"
          doStep 4 msg
        else storyWithChooseOneM' (setTitle "title" >> p "friendsInLowPlaces1") do
          labeled' "thankCharlie" $ doStep 2 msg
          labeled' "inviteCharlie" $ doStep 3 msg
      nextCampaignStep
      pure c
    DoStep 2 (CampaignStep (InterludeStep 1 _)) -> scope "friendsInLowPlaces" do
      flavor $ setTitle "title" >> p "friendsInLowPlaces2"
      eachInvestigator \iid -> gainXp iid CampaignSource (ikey "xp.friendsInLowPlaces") 2
      pure c
    DoStep 3 interlude@(CampaignStep (InterludeStep 1 _)) -> scope "friendsInLowPlaces" do
      settledJulia <-
        orM [getHasRecord InvestigatorsSparedJuliaStern, getHasRecord InvestigatorsKilledJuliaStern]
      completedSearch <- getHasRecord InvestigatorsCompletedTheirSearch
      storyCards <- getCampaignStoryCards
      let blighted = any (any ((== Treacheries.theBloodBlight) . toCardDef)) (toList storyCards)
      flavor do
        setTitle "title"
        p "friendsInLowPlaces3"
        p.basic "friendsInLowPlaces3Check"
        ul do
          li.validate settledJulia "juliaWasSettled"
          li.validate completedSearch "completedTheirSearch"
          li.validate (not blighted) "noBloodBlight"
      doStep (if settledJulia && completedSearch && not blighted then 5 else 6) interlude
      pure c
    DoStep 4 (CampaignStep (InterludeStep 1 _)) -> scope "friendsInLowPlaces" do
      flavor $ setTitle "title" >> p "friendsInLowPlaces4"
      eachInvestigator (`sufferMentalTrauma` 1)
      pure c
    DoStep 5 (CampaignStep (InterludeStep 1 _)) -> scope "friendsInLowPlaces" do
      flavor $ setTitle "title" >> p "friendsInLowPlaces5"
      investigators <- allInvestigators
      addCampaignCardToDeckChoice investigators DoNotShuffleIn Assets.charlieKaneKnowsAGuy
      pure c
    DoStep 6 (CampaignStep (InterludeStep 1 _)) -> scope "friendsInLowPlaces" do
      flavor $ setTitle "title" >> p "friendsInLowPlaces6"
      pure c
    FailedSkillTest iid _ _ (Initiator _) _ _ -> sealBloodOnFailure iid >> pure c
    After (FailedSkillTest iid _ _ (Initiator _) _ _) -> sealBloodOnFailure iid >> pure c
    _ -> lift $ defaultCampaignRunner msg c

{- | Additional rules: the first {blood} revealed during a skill test reads "-1.
Reveal another token. If this skill test fails, seal 1 {blood} token revealed
during this test on the performing investigator." No more than 3 may be sealed on
one investigator. Both failure paths dispatch a once-per-test message, so this
re-checks the token instead of assuming it runs once.
-}
sealBloodOnFailure :: ReverseQueue m => InvestigatorId -> m ()
sealBloodOnFailure iid = do
  bloods <- filter ((== #blood) . (.face)) <$> getSkillTestRevealedChaosTokens
  for_ (headMay bloods) \token -> do
    sealed <- select $ SealedOnInvestigator (InvestigatorWithId iid) #blood
    when (token `notElem` sealed && length sealed < 3) $ sealChaosToken iid iid token
