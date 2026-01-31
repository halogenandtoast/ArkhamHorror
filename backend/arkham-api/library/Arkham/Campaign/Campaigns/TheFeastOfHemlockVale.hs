module Arkham.Campaign.Campaigns.TheFeastOfHemlockVale (theFeastOfHemlockVale) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Import.Lifted
import Arkham.Campaigns.TheFeastOfHemlockVale.CampaignSteps
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Campaigns.TheFeastOfHemlockVale.Key
import Arkham.ChaosToken
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query
import Arkham.Helpers.Xp
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log

newtype TheFeastOfHemlockVale = TheFeastOfHemlockVale CampaignAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor)

theFeastOfHemlockVale :: Difficulty -> TheFeastOfHemlockVale
theFeastOfHemlockVale = campaign TheFeastOfHemlockVale (CampaignId "10") "The Feast of Hemlock Vale"

{- FOURMOLU_DISABLE -}
campaignChaosBag :: Difficulty -> [ChaosTokenFace]
campaignChaosBag = \case
  Easy ->
    [ #"+1", #"+1", #"0", #"0", #"0", #"-1", #"-1", #"-1", #"-2", #"-2", #"-3"
    , Skull, Skull, ElderThing
    ]
  Standard ->
    [ #"+1", #"0", #"0", #"-1", #"-1", #"-1", #"-2", #"-2", #"-3", #"-3", #"-4"
    , Skull, Skull, ElderThing
    ]
  Hard ->
    [ #"0", #"0", #"0", #"-1", #"-1", #"-2", #"-2", #"-3", #"-3", #"-5", #"-5", #"-7"
    , Skull, Skull, ElderThing
    ]
  Expert ->
    [ #"0", #"-1", #"-1", #"-2", #"-2", #"-3", #"-3", #"-4", #"-5", #"-5", #"-6", #"-6", #"-8"
    , Skull, Skull, ElderThing
    ]
{- FOURMOLU_ENABLE -}

instance IsCampaign TheFeastOfHemlockVale where
  campaignTokens = campaignChaosBag
  nextStep a = case (campaignStep (toAttrs a)).normalize of
    PrologueStep -> continue PreludeWelcomeToHemlockVale
    WrittenInRock ->
      let meta = toResult @TheFeastOfHemlockValeMeta (toAttrs a).meta
       in case (meta.day, meta.time) of
            (Day1, Day) -> continueEdit (CampaignSpecificStep "preludeTheFirstEvening" Nothing) allowOptions
            (Day1, Night) -> Nothing
            (Day2, Day) -> Nothing
            (Day2, Night) -> Nothing
            (Day3, Day) -> Nothing
            (Day3, Night) -> Nothing
    EpilogueStep -> Nothing
    UpgradeDeckStep nextStep' -> Just nextStep'
    _ -> Nothing

instance RunMessage TheFeastOfHemlockVale where
  runMessage msg c@(TheFeastOfHemlockVale attrs) = runQueueT $ campaignI18n $ case msg of
    CampaignStep PrologueStep -> do
      scope "prologue" $ flavor $ setTitle "title" >> p "body"
      scope "additionalRulesAndClarifications" do
        flavor $ setTitle "title" >> p "theCodex"
        flavor $ setTitle "title" >> p "threeDaysThreeNights"
        flavor $ setTitle "title" >> p "residentsAndRelationshipLevels"
        flavor $ setTitle "title" >> p "locationAdjacency"
      scope "newKeywords" $ flavor $ setTitle "title" >> p "body"
      nextCampaignStep
      pure $ TheFeastOfHemlockVale $ attrs & metaL .~ toJSON initMeta
    CampaignStep (CampaignSpecificStep "preludeTheFirstEvening" Nothing) -> do
      scope "prelude.theFirstEvening" do
        flavor $ setTitle "title" >> p "body"
        eachInvestigator \iid -> do
          investigatorStoryWithChooseOneM' iid (setTitle "title" >> p "mealChoice") do
            labeled' "keepEating" $ recordForInvestigator iid FinishedTheirMeal
            labeled' "dontEatAnymore" nothing
      n <- getPlayerCount
      eachInvestigator (`forInvestigator` msg)
      when (n == 1) $ eachInvestigator (`forInvestigator` msg)
      doStep 1 msg
      nextCampaignStep
      let meta = toResultDefault initMeta attrs.meta
      let meta' = meta {chosenCodexEntries = []}
      pure $ TheFeastOfHemlockVale $ attrs & metaL .~ toJSON meta'
    DoStep 1 (CampaignStep (CampaignSpecificStep "preludeTheFirstEvening" Nothing)) -> do
      scope "prelude.theFirstEvening" do
        resolutionFlavorWithChooseOne (setTitle "resolution1.title" >> p "resolution1.body") do
          labeled' "searchForBertie" $ setNextCampaignStep TheTwistedHollow
          labeled' "gatherMoreInformation" nothing
      pure c
    ForInvestigator iid (CampaignStep (CampaignSpecificStep "preludeTheFirstEvening" Nothing)) -> do
      scope "prelude.theFirstEvening" do
        n <- getPlayerCount
        let meta = toResultDefault initMeta attrs.meta
        let option k =
              labeledValidate' (k `notElem` meta.chosenCodexEntries) k
                $ forInvestigator iid
                $ CampaignStep (CampaignSpecificStep "preludeTheFirstEvening" (Just k))
        investigatorStoryWithChooseOneM' iid (setTitle "title" >> p "codexChoice") do
          option "theta"
          option "delta"
          option "sigma"
          option "omega"
          option "gamma"
          labeledValidate'
            ("pi" `notElem` meta.chosenCodexEntries && (n > 1 || notNull meta.chosenCodexEntries))
            "pi"
            $ forInvestigator iid
            $ CampaignStep (CampaignSpecificStep "preludeTheFirstEvening" (Just "pi"))
      pure c
    ForInvestigator iid (CampaignStep (CampaignSpecificStep "preludeTheFirstEvening" (Just entry))) -> do
      scope "prelude.theFirstEvening" do
        case entry of
          "theta" -> do
            investigatorStoryWithChooseOneM' iid (setTitle "title" >> p "theHemlockLegacy1") do
              labeled' "whoAreTheAtwoods" do
                incrementRecordCount MotherRachelRelationshipLevel 1
                interludeXpAll (toBonus "bonus" 1)
                flavor $ setTitle "title" >> p "theHemlockLegacy2"
              labeled' "whoAreTheChildrenOfTheStars" do
                incrementRecordCount MotherRachelRelationshipLevel 1
                interludeXpAll (toBonus "bonus" 1)
                flavor $ setTitle "title" >> p "theHemlockLegacy3"
              labeled' "whoAreTheHemlocks" do
                incrementRecordCount WilliamHemlockRelationshipLevel 1
                interludeXpAll (toBonus "bonus" 1)
                record WilliamSharedHisLegacy
                flavor $ setTitle "title" >> p "theHemlockLegacy4"
          "delta" -> do
            investigatorStoryWithChooseOneM' iid (setTitle "title" >> p "bestFriends1") do
              labeled' "jazzIsDelightful" do
                incrementRecordCount RiverHawthorneRelationshipLevel 1
                interludeXpAll (toBonus "bonus" 1)
                flavor $ setTitle "title" >> p "bestFriends2"
              labeled' "jazzIsHell" do
                incrementRecordCount JudithParkRelationshipLevel 1
                interludeXpAll (toBonus "bonus" 1)
                flavor $ setTitle "title" >> p "bestFriends3"
          "sigma" -> do
            searched <- getHasRecord LeahSearchedThePearlRuins
            investigatorStoryWithChooseOneM' iid (setTitle "title" >> p "oldBlood1") do
              labeled' "gideon" do
                incrementRecordCount GideonMizrahRelationshipLevel 1
                record GideonToldTheStoryOfCaptainHemlock
                interludeXpAll (toBonus "bonus" 1)
                flavor $ setTitle "title" >> p "oldBlood2"
              labeled' "leah" do
                incrementRecordCount JudithParkRelationshipLevel 1
                interludeXpAll (toBonus "bonus" 1)
                flavor $ setTitle "title" >> p "oldBlood3"
                when searched do
                  record LeahSharedHerFrustrations
                  flavor $ setTitle "title" >> p "oldBlood4"
          "omega" -> do
            incrementRecordCount TheoPetersRelationshipLevel 1
            reunited <- getHasRecord ThePetersFamilyWereReunited
            flavor do
              setTitle "title"
              compose.green do
                h3 "familyMatters.header"
                p.validate reunited "familyMatters.reunited"
                hr
                p.validate (not reunited) "familyMatters.otherwise"
            when reunited do
              record HelenPetersJoinedTheSurvey
              addCampaignCardToDeckChoice_ Assets.helenPetersTheEldestSister
          "gamma" -> do
            incrementRecordCount TheoPetersRelationshipLevel 1
            simeonCrossedOut <- getHasRecord SimeonCrossedOut
            unless simeonCrossedOut do
              incrementRecordCount SimeonAtwoodRelationshipLevel 1
              record SimeonHatchedAPlan

            interludeXpAll (toBonus "bonus" 1)
            flavor do
              setTitle "title"
              compose.green do
                h3 "theRabbit.header"
                p.validate simeonCrossedOut "theRabbit.simeonCrossedOut"
                hr
                p.validate (not simeonCrossedOut) "theRabbit.otherwise"
          "pi" -> do
            flavor $ setTitle "title" >> p "friendlyStrangers"
            addCampaignCardToDeck iid DoNotShuffleIn Assets.worryRockTokenOfSafety
          _ -> error "Unknown codex"
      let meta = toResultDefault initMeta attrs.meta
      let meta' = meta {chosenCodexEntries = entry : meta.chosenCodexEntries}
      pure $ TheFeastOfHemlockVale $ attrs & metaL .~ toJSON meta'
    _ -> lift $ defaultCampaignRunner msg c
