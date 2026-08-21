module Arkham.Campaign.Campaigns.ChildrenOfBlood (childrenOfBlood) where

import Arkham.Campaign.Import.Lifted
import Arkham.Campaigns.ChildrenOfBlood.CampaignSteps
import Arkham.Campaigns.ChildrenOfBlood.ChaosBag
import Arkham.Campaigns.ChildrenOfBlood.Helpers
import Arkham.Helpers.FlavorText
import Arkham.Helpers.SkillTest (getSkillTestRevealedChaosTokens)
import Arkham.Matcher
import Arkham.Target (pattern Initiator)

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
