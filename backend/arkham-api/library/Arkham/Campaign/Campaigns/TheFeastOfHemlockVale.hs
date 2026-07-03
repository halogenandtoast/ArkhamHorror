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
import Arkham.Matcher.Investigator
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Source

newtype TheFeastOfHemlockVale = TheFeastOfHemlockVale CampaignAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor)

theFeastOfHemlockVale :: Difficulty -> TheFeastOfHemlockVale
theFeastOfHemlockVale = campaign TheFeastOfHemlockVale (CampaignId "10") "The Feast of Hemlock Vale"

{- FOURMOLU_DISABLE -}
campaignChaosBag :: Difficulty -> [ChaosTokenFace]
campaignChaosBag = \case
  Easy ->
    [ #"+1", #"+1", #"0", #"0", #"0", #"-1", #"-1", #"-1", #"-2", #"-2", #"-3"
    , Skull, Skull, ElderSign
    ]
  Standard ->
    [ #"+1", #"0", #"0", #"-1", #"-1", #"-1", #"-2", #"-2", #"-3", #"-3", #"-4"
    , Skull, Skull, ElderSign
    ]
  Hard ->
    [ #"0", #"0", #"0", #"-1", #"-1", #"-2", #"-2", #"-3", #"-3", #"-5", #"-5", #"-7"
    , Skull, Skull, ElderSign
    ]
  Expert ->
    [ #"0", #"-1", #"-1", #"-2", #"-2", #"-3", #"-3", #"-4", #"-5", #"-5", #"-6", #"-7", #"-8"
    , Skull, Skull, ElderSign
    ]
{- FOURMOLU_ENABLE -}

instance IsCampaign TheFeastOfHemlockVale where
  campaignTokens = campaignChaosBag
  nextStep a =
    let meta = toResult @TheFeastOfHemlockValeMeta (toAttrs a).meta
        handleTime = do
          case (meta.day, meta.time) of
            (Day1, Day) -> continueEdit (CampaignSpecificStep "preludeTheFirstEvening" Nothing) allowOptions
            (Day1, Night) -> continue PreludeDawnOfTheSecondDay
            (Day2, Day) -> continueEdit (CampaignSpecificStep "preludeTheSecondEvening" Nothing) allowOptions
            (Day2, Night) -> Nothing
            (Day3, _) -> continueEdit PreludeTheFinalEvening allowOptions
     in case (campaignStep (toAttrs a)).normalize of
          PrologueStep -> continue PreludeWelcomeToHemlockVale
          WrittenInRock -> handleTime
          HemlockHouse -> handleTime
          TheSilentHeath -> handleTime
          TheLostSister -> handleTime
          TheThingInTheDepths -> handleTime
          TheTwistedHollow -> continue PreludeDawnOfTheSecondDay
          TheLongestNight -> continue PreludeDawnOfTheFinalDay
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
      let meta = toResultDefault initMeta attrs.meta
      let meta' = meta {chosenCodexEntries = [], time = Night}
      pure $ TheFeastOfHemlockVale $ attrs & metaL .~ toJSON meta'
    DoStep 1 (CampaignStep (CampaignSpecificStep "preludeTheFirstEvening" Nothing)) -> do
      scope "prelude.theFirstEvening" do
        resolutionFlavorWithChooseOne (setTitle "resolution1.title" >> p "resolution1.body") do
          labeled' "searchForBertie" $ setNextCampaignStep TheTwistedHollow
          labeled' "gatherMoreInformation" do
            addChaosToken #tablet
            addChaosToken #elderthing
            areas <- getAreasSurveyed
            let survey k = unless (k `elem` areas)
            leadChooseOneM do
              questionLabeled' "survey"
              survey NorthPointMine do
                scenarioLabeled' "writtenInRock" "10501-night1" $ campaignStepEdit_ WrittenInRock noUpgrade
              survey HemlockHarbor do
                scenarioLabeled' "hemlockHouse" "10523-night1" $ campaignStepEdit_ HemlockHouse noUpgrade
              survey PearlRidge do
                scenarioLabeled' "theSilentHeath" "10549-night1" $ campaignStepEdit_ TheSilentHeath noUpgrade
              survey AkwanShoreline do
                scenarioLabeled' "theLostSister" "10569-night1" $ campaignStepEdit_ TheLostSister noUpgrade
              survey EastwickBog do
                scenarioLabeled' "theThingInTheDepths" "10588-night1" $ campaignStepEdit_ TheThingInTheDepths noUpgrade
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
    CampaignStep (CampaignSpecificStep "preludeTheSecondEvening" Nothing) -> do
      scope "prelude.theSecondEvening" do
        simeonSurvived <- getHasRecord SimeonSurvived
        flavor do
          setTitle "title"
          p "theSecondEvening1"
          unscoped $ p.basic "checkCampaignLog"
          ul do
            li.validate simeonSurvived "simeonSurvived"
            li.validate (not simeonSurvived) "simeonDidNotSurvive"
        if simeonSurvived
          then flavor $ setTitle "title" >> p "theSecondEvening2"
          else do
            flavor $ setTitle "title" >> p "theSecondEvening3"
            record LeahIsSearchingForSimeon
            record SimeonCrossedOut
        flavor $ setTitle "title" >> p "theSecondEvening4"
      n <- getPlayerCount
      eachInvestigator (`forInvestigator` msg)
      when (n == 1) $ eachInvestigator (`forInvestigator` msg)
      doStep 1 msg
      let meta = toResultDefault initMeta attrs.meta
      let meta' = meta {chosenCodexEntries = [], time = Night}
      pure $ TheFeastOfHemlockVale $ attrs & metaL .~ toJSON meta'
    DoStep 1 (CampaignStep (CampaignSpecificStep "preludeTheSecondEvening" Nothing)) -> do
      scope "prelude.theSecondEvening" do
        flavor $ setTitle "resolution1.title" >> p "resolution1.body"
        diaryRecovered <- getHasRecord MadamePearlsDiaryWasRecovered
        when diaryRecovered do
          flavor $ setTitle "resolution2.title" >> p "resolution2.body"
          addCampaignCardToDeckChoice_ Assets.thePearlDiaryAGrimAccount
        flavor $ setTitle "resolution3.title" >> p "resolution3.body"
        bertieRescued <- getHasRecord BertieWasRescued
        if bertieRescued
          then flavor $ setTitle "resolution4.title" >> p "resolution4.body"
          else do
            flavor $ setTitle "resolution5.title" >> p "resolution5.body"
            record BertieHadAnEpiphany
        resolutionFlavorWithChooseOne (setTitle "resolution6.title" >> p "resolution6.body") do
          labeled' "followDrMarquez" $ setNextCampaignStep TheLongestNight
          labeled' "gatherMoreInformation" do
            addChaosToken Skull
            addChaosToken #cultist
            addChaosToken ElderThing
            areas <- getAreasSurveyed
            let survey k = unless (k `elem` areas)
            leadChooseOneM do
              questionLabeled' "survey"
              survey NorthPointMine do
                scenarioLabeled' "writtenInRock" "10501-night2" $ campaignStepEdit_ WrittenInRock noUpgrade
              survey HemlockHarbor do
                scenarioLabeled' "hemlockHouse" "10523-night2" $ campaignStepEdit_ HemlockHouse noUpgrade
              survey PearlRidge do
                scenarioLabeled' "theSilentHeath" "10549-night2" $ campaignStepEdit_ TheSilentHeath noUpgrade
              survey AkwanShoreline do
                scenarioLabeled' "theLostSister" "10569-night2" $ campaignStepEdit_ TheLostSister noUpgrade
              survey EastwickBog do
                scenarioLabeled' "theThingInTheDepths" "10588-night2" $ campaignStepEdit_ TheThingInTheDepths noUpgrade
      pure c
    ForInvestigator iid (CampaignStep (CampaignSpecificStep "preludeTheSecondEvening" Nothing)) -> do
      scope "prelude.theSecondEvening" do
        let meta = toResultDefault initMeta attrs.meta
        crossedOut <- getCrossedOutResidents
        let residentOption r k =
              labeledValidate' (r `notElem` crossedOut && k `notElem` meta.chosenCodexEntries) k
                $ forInvestigator iid
                $ CampaignStep (CampaignSpecificStep "preludeTheSecondEvening" (Just k))
        let option k =
              labeledValidate' (k `notElem` meta.chosenCodexEntries) k
                $ forInvestigator iid
                $ CampaignStep (CampaignSpecificStep "preludeTheSecondEvening" (Just k))
        investigatorStoryWithChooseOneM' iid (setTitle "title" >> p "codexChoice") do
          residentOption MotherRachel "motherRachel"
          residentOption LeahAtwood "leah"
          residentOption SimeonAtwood "simeon"
          residentOption WilliamHemlock "william"
          residentOption RiverHawthorne "river"
          residentOption GideonMizrah "gideon"
          residentOption JudithPark "judith"
          residentOption TheoPeters "theo"
          option "alone"
          option "omega"
          option "theta"
      pure c
    ForInvestigator _iid (CampaignStep (CampaignSpecificStep "preludeTheSecondEvening" (Just entry))) -> do
      scope "prelude.theSecondEvening" do
        case entry of
          "motherRachel" -> do
            flavor do
              compose.green do
                h3 "codex1.header"
                p "codex1.body"
            record MotherRachelSharedHerDoubts
          "leah" -> do
            leahSearching <- getHasRecord LeahIsSearchingForSimeon
            flavor do
              compose.green do
                h3 "codex2.header"
                p.validate leahSearching "codex2.searching"
                hr
                p.validate (not leahSearching) "codex2.otherwise"
            unless leahSearching $ record LeahSharedADance
          "simeon" -> do
            level <- getRelationshipLevel SimeonAtwood
            flavor do
              compose.green do
                h3 "codex3.header"
                p.validate (level >= 3) "codex3.highLevel"
                hr
                p.validate (level < 3) "codex3.otherwise"
            when (level >= 3) $ record SimeonSharedADance
          "william" -> do
            level <- getRelationshipLevel WilliamHemlock
            flavor do
              compose.green do
                h3 "codex4.header"
                p.validate (level >= 3) "codex4.highLevel"
                hr
                p.validate (level < 3) "codex4.otherwise"
            when (level >= 3) $ record WilliamSharedADance
          "river" -> do
            level <- getRelationshipLevel RiverHawthorne
            flavor do
              compose.green do
                h3 "codex5.header"
                p.validate (level >= 3) "codex5.highLevel"
                hr
                p.validate (level < 3) "codex5.otherwise"
            when (level >= 3) $ record RiverSharedADance
          "gideon" -> do
            level <- getRelationshipLevel GideonMizrah
            flavor do
              compose.green do
                h3 "codex6.header"
                p.validate (level >= 3) "codex6.highLevel"
                hr
                p.validate (level < 3) "codex6.otherwise"
            when (level >= 3) $ record GideonSharedADance
          "judith" -> do
            level <- getRelationshipLevel JudithPark
            flavor do
              compose.green do
                h3 "codex7.header"
                p.validate (level >= 3) "codex7.highLevel"
                hr
                p.validate (level < 3) "codex7.otherwise"
            when (level >= 3) $ record JudithSharedADance
          "theo" -> do
            level <- getRelationshipLevel TheoPeters
            flavor do
              compose.green do
                h3 "codex8.header"
                p.validate (level >= 3) "codex8.highLevel"
                hr
                p.validate (level < 3) "codex8.otherwise"
            when (level >= 3) $ record TheoSharedADance
          "alone" -> do
            flavor do
              compose.green do
                h3 "codex9.header"
                p "codex9.body"
            interludeXpAll (toBonus "bonus" 1)
          "omega" -> do
            helenJoined <- getHasRecord HelenPetersJoinedTheSurvey
            reunited <- getHasRecord ThePetersFamilyWereReunited
            flavor do
              compose.green do
                h3 "codexOmega.header"
                p.validate helenJoined "codexOmega.helenJoined"
                hr
                p.validate (not helenJoined && reunited) "codexOmega.reunited"
                hr
                p.validate (not helenJoined && not reunited) "codexOmega.otherwise"
            if helenJoined
              then record HelenSharedADance
              else
                if reunited
                  then do
                    record HelenPetersJoinedTheSurvey
                    addCampaignCardToDeckChoice_ Assets.helenPetersTheEldestSister
                  else record TheoRejectedHisFamily
          "theta" -> do
            flavor do
              compose.green do
                h3 "codexTheta.header"
                p "codexTheta.body"
            eachInvestigator \iid' -> gainResources iid' CampaignSource 2
          _ -> error "Unknown codex"
      let meta = toResultDefault initMeta attrs.meta
      let meta' = meta {chosenCodexEntries = entry : meta.chosenCodexEntries}
      pure $ TheFeastOfHemlockVale $ attrs & metaL .~ toJSON meta'
    -- The Dawn of the Final Day prelude handles each investigator killed in The
    -- Longest Night itself (letting their player pick a new investigator and
    -- awarding half experience), so we skip the standard killed/insane handling
    -- as we transition into it.
    HandleKilledOrInsaneInvestigators | PreludeDawnOfTheFinalDay <- (campaignStep attrs).unwrap.normalize -> pure c
    NextCampaignStep mOverrideStep -> do
      let mstep = mOverrideStep <|> nextStep c
      let meta = toResultDefault initMeta attrs.meta
      let
        meta' =
          case mstep of
            Just PreludeDawnOfTheSecondDay -> meta {day = Day2, time = Day}
            Just PreludeDawnOfTheFinalDay -> meta {day = Day3, time = Day}
            Just PreludeTheFinalEvening -> meta {day = Day3, time = Night}
            _ -> meta
      TheFeastOfHemlockVale attrs' <- lift $ defaultCampaignRunner msg c
      pure $ TheFeastOfHemlockVale $ attrs' & metaL .~ toJSON meta'
    CampaignStep EpilogueStep -> scope "epilogue" do
      anyAliveInvestigators <- selectAny AliveInvestigator
      let meta = toResultDefault initMeta attrs.meta
      marquezSacrificedHerself <- getHasRecord DrMarquezSacrificedHerselfForTheVale
      investigatorsSacrificedThemselves <- getHasRecord TheInvestigatorsSacrificedThemselvesForTheVale
      flavor do
        setTitle "title"
        p "body"
        ul do
          li.validate marquezSacrificedHerself "marquezSacrificedHerself"
          li.validate investigatorsSacrificedThemselves "investigatorsSacrificedThemselves"
          li.validate (not marquezSacrificedHerself && not investigatorsSacrificedThemselves) "otherwise"
      flavor do
        setTitle "title"
        p
          $ if
            | marquezSacrificedHerself -> "epilogue1"
            | investigatorsSacrificedThemselves -> "epilogue2"
            | otherwise -> "epilogue3"
      when (anyAliveInvestigators && not investigatorsSacrificedThemselves) do
        push $ CampaignStep $ CampaignSpecificStep "epilogueCodex" Nothing
      gameOver
      pure $ TheFeastOfHemlockVale $ attrs & metaL .~ toJSON (meta {chosenCodexEntries = []})
    CampaignStep (CampaignSpecificStep "epilogueCodex" Nothing) -> scope "epilogue" do
      let meta = toResultDefault initMeta attrs.meta
      leahCrossedOut <- getHasRecord LeahCrossedOut
      simeonCrossedOut <- getHasRecord SimeonCrossedOut
      hemlocksMadeATruce <- getHasRecord TheHemlocksMadeATruce
      williamCrossedOut <- getHasRecord WilliamCrossedOut
      riverCrossedOut <- getHasRecord RiverCrossedOut
      judithCrossedOut <- getHasRecord JudithCrossedOut
      theoCrossedOut <- getHasRecord TheoCrossedOut
      judithLevel <- getRelationshipLevel JudithPark
      theoLevel <- getRelationshipLevel TheoPeters
      let atwoods = not leahCrossedOut && not simeonCrossedOut && "atwoods" `notElem` meta.chosenCodexEntries
      let hemlocks =
            hemlocksMadeATruce
              && not williamCrossedOut
              && not riverCrossedOut
              && "hemlocks"
              `notElem` meta.chosenCodexEntries

      let judith = judithLevel >= 5 && not judithCrossedOut && "judith" `notElem` meta.chosenCodexEntries
      let theo = theoLevel >= 5 && not theoCrossedOut && "theo" `notElem` meta.chosenCodexEntries
      when (atwoods || hemlocks || judith || theo) do
        leadChooseOneM do
          labeledValidate' atwoods "atwoods"
            $ push
            $ CampaignStep
            $ CampaignSpecificStep "epilogueCodex" (Just "atwoods")
          labeledValidate' hemlocks "hemlocks"
            $ push
            $ CampaignStep
            $ CampaignSpecificStep "epilogueCodex" (Just "hemlocks")
          labeledValidate' judith "judith"
            $ push
            $ CampaignStep
            $ CampaignSpecificStep "epilogueCodex" (Just "judith")
          labeledValidate' theo "theo"
            $ push
            $ CampaignStep
            $ CampaignSpecificStep "epilogueCodex" (Just "theo")
          unscoped skip_
      pure c
    CampaignStep (CampaignSpecificStep "epilogueCodex" (Just entry)) -> scope "epilogue.codex" do
      case entry of
        "atwoods" -> scope "atwoods" $ flavor $ setTitle "title" >> p "body"
        "hemlocks" -> scope "hemlocks" $ flavor $ setTitle "title" >> p "body"
        "judith" -> scope "judith" $ flavor $ setTitle "title" >> p "body"
        "theo" -> scope "theo" $ flavor $ setTitle "title" >> p "body"
        _ -> error "Unknown epilogue codex"
      push $ CampaignStep $ CampaignSpecificStep "epilogueCodex" Nothing
      let meta = toResultDefault initMeta attrs.meta
      let meta' = meta {chosenCodexEntries = entry : meta.chosenCodexEntries}
      pure $ TheFeastOfHemlockVale $ attrs & metaL .~ toJSON meta'
    _ -> lift $ defaultCampaignRunner msg c
