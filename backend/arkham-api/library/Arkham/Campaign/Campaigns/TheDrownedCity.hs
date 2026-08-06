module Arkham.Campaign.Campaigns.TheDrownedCity (theDrownedCity) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Campaigns.TheDrownedCity.Achievements (runDrownedCityAchievements)
import Arkham.Campaign.Import.Lifted
import Arkham.Campaigns.TheDrownedCity.CampaignSteps
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.Card
import Arkham.Card.PlayerCard (lookupPlayerCard)
import Arkham.ChaosToken
import Arkham.Helpers.Campaign (getTakenBasicWeaknesses, replaceCampaignChaosTokens)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.I18n (ikey)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.PlayerCard (allPlayerCards)
import Arkham.Source (Source (CampaignSource))
import Arkham.Trait (Trait (Agency, Criminal, Detective, Injury))
import Arkham.Window qualified as Window
import Data.List.Extra (nubOrdOn)
import Data.Text qualified as T

newtype TheDrownedCity = TheDrownedCity CampaignAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Walk in Faith's failure: "For the remainder of the campaign, you must treat
the {elderThing} token as if it were a {tablet} token, instead."

The substitution has to land on the chaos tokens that investigator reveals, not on
the investigator: skill test resolution reads 'ForcedChaosTokenChange' off
'ChaosTokenTarget' (via @getModifiedChaosTokenFace@), and the investigator-target
copy only feeds the client's display. It also has to outlive every scenario, so it
is derived from the Campaign Log record rather than pushed as a modifier once.
-}
instance HasModifiersFor TheDrownedCity where
  getModifiersFor (TheDrownedCity a) = do
    getModifiersFor a
    modifySelect
      CampaignSource
      (ChaosTokenRevealedBy $ investigatorWithRecord LostTheirFaith)
      [ForcedChaosTokenChange ElderThing [Tablet]]

theDrownedCity :: Difficulty -> TheDrownedCity
theDrownedCity = campaign TheDrownedCity (CampaignId "11") "The Drowned City"

instance IsCampaign TheDrownedCity where
  campaignTokens = chaosBagContents
  nextStep a = case (toAttrs a).normalizedStep of
    PrologueStep -> continue OneLastJob
    OneLastJob -> continue AnOfferYouCantRefuse
    AnOfferYouCantRefuse -> continue ExpeditionToRlyeh
    Finale -> continue TheDoomOfArkhamPartI
    -- The west/east branch out of Expedition to R'lyeh is pushed explicitly from
    -- that interlude's handler via setNextCampaignStep.
    other -> defaultNextStep other

completedTask :: TheDrownedCityKey -> Maybe CardDef
completedTask = \case
  WalkInFaith -> Just Assets.walkInFaithCompleted
  GoodMoney -> Just Assets.goodMoneyCompleted
  DreamsOfDestruction -> Just Assets.dreamsOfDestructionCompleted
  NoPlaceLikeHome -> Just Assets.noPlaceLikeHomeCompleted
  DoNoHarm -> Just Assets.doNoHarmCompleted
  ToeTheLine -> Just Assets.toeTheLineCompleted
  ProveYourWorth -> Just Assets.proveYourWorthCompleted
  PlumbTheDepths -> Just Assets.plumbTheDepthsCompleted
  _ -> Nothing

completedTaskRecord :: TheDrownedCityKey -> Maybe TheDrownedCityKey
completedTaskRecord = \case
  WalkInFaith -> Just IsStrongInTheirFaith
  GoodMoney -> Just MadeBank
  DreamsOfDestruction -> Just UnderstandsTheFuture
  NoPlaceLikeHome -> Just FoundTheirTrueHome
  DoNoHarm -> Just SworeAnOathToProtectOthers
  ToeTheLine -> Just FoundNewWork
  ProveYourWorth -> Just PulledTheirWeight
  PlumbTheDepths -> Just LearnedTheSecretTruth
  _ -> Nothing

{- | A Task's Return to Arkham entry: the completed half or the failed half,
depending on how much progress was marked. Written against the @tasks.<label>@
scope, so call it under one.

Factored out because No Place Like Home's failure folds a choice into this same
story rather than asking separately afterwards.
-}
taskStory :: HasI18n => Bool -> FlavorTextBuilder ()
taskStory completed = compose.green do
  h3 "title"
  p.basic "readOnly"
  compose.validate completed do
    p "fiveOrMoreProgress"
    p "completed"
    ul $ li "completedEffect"
  hr
  compose.validate (not completed) do
    p "otherwise"
    p "failed"
    ul $ li "failedEffect"

{- | The chaos token two values lower, for Toe the Line's failure, or Nothing if it
cannot be lowered that far (a symbol token, or -7/-8, which have no -9/-10 below
them).
-}
lowerChaosTokenByTwo :: ChaosTokenFace -> Maybe ChaosTokenFace
lowerChaosTokenByTwo = \case
  PlusOne -> Just MinusOne
  Zero -> Just MinusTwo
  MinusOne -> Just MinusThree
  MinusTwo -> Just MinusFour
  MinusThree -> Just MinusFive
  MinusFour -> Just MinusSix
  MinusFive -> Just MinusSeven
  MinusSix -> Just MinusEight
  _ -> Nothing

instance RunMessage TheDrownedCity where
  runMessage msg c =
    runQueueT $ campaignI18n $ lift (runDrownedCityAchievements msg) *> case msg of
      StartCampaign -> do
        -- The R'lyeh map starts with every scenario on it; each is crossed out as
        -- that scenario is completed.
        recordSetInsert RlyehMap $ map toJSON [minBound @RlyehMapEntry ..]
        lift $ defaultCampaignRunner msg c
      CampaignStep PrologueStep -> do
        -- Intro: the epigraph and the April 15 journal entry that open the campaign.
        scope "intro" $ flavor $ setTitle "title" >> p "body"
        -- The Prologue, as its own thing after the intro: the campaign's additional
        -- rules and new keywords, before the first scenario.
        scope "additionalRulesAndClarifications" do
          flavor $ setTitle "title" >> p "floodTokens"
          flavor $ setTitle "title" >> p "artifacts"
          flavor $ setTitle "title" >> p "alienGlyphs"
        scope "newKeywords" $ flavor $ setTitle "title" >> p "body"
        scope "prologue" $ flavor $ setTitle "title" >> p "body"
        nextCampaignStep
        pure c
      CampaignStep (InterludeStep 1 _) -> scope "anOfferYouCantRefuse" do
        agencyDetectiveOrCriminal <-
          selectAny $ mapOneOf InvestigatorWithTrait [Agency, Detective, Criminal]
        flavor do
          setTitle "title"
          p "interlude1"
          p.validate agencyDetectiveOrCriminal "agencyDetectiveOrCriminal"
          p "interlude1Continued"
        eachInvestigator (`forInvestigator` msg)
        doStep 2 msg
        pure c
      ForInvestigator iid (CampaignStep (InterludeStep 1 _)) -> scope "anOfferYouCantRefuse" do
        takenFlags <- traverse (\(k, _, _) -> getHasRecord k) tasks
        let isTaken key = or [t | ((k, _, _), t) <- zip tasks takenFlags, k == key]
        investigatorStoryWithChooseOneM' iid (setTitle "title" >> p "chooseTask") do
          for_ tasks \(key, cardDef, lbl) ->
            labeledValidate' (not (isTaken key)) lbl do
              record key
              addCampaignCardToDeck iid DoNotShuffleIn cardDef
              scope "task" $ scope lbl $ flavor $ setTitle "title" >> p "body"
        pure c
      DoStep 2 (CampaignStep (InterludeStep 1 _)) -> scope "anOfferYouCantRefuse" do
        storyWithChooseOneM' (setTitle "title" >> p "interlude2") do
          labeled' "refuse" do
            flavor $ setTitle "title" >> p "interlude3"
            gameOver
          labeled' "accept" do
            flavor $ setTitle "title" >> p "interlude4"
            nextCampaignStep
        pure c
      CampaignStep (InterludeStep 2 _) -> scope "expeditionToRlyeh" do
        storyWithChooseOneM' (setTitle "title" >> p "body") do
          labeled' "west" do
            record TheExpeditionHeadedWest
            flavor do
              setTitle "title"
              p "westernExpedition"
              ul do
                li "theExpeditionHeadedWest"
                li "andyVanNortwick"
                li "westernChaosTokens"
                li "proceedToTheWesternWall"
            addCampaignCardToDeckChoice_ =<< genPlayerCard Assets.andyVanNortwick
            setNextCampaignStep TheWesternWall
          labeled' "east" do
            record TheExpeditionHeadedEast
            flavor do
              setTitle "title"
              p "easternExpedition"
              ul do
                li "theExpeditionHeadedEast"
                li "rubyStandish"
                li "proceedToObsidianCanyons"
            addCampaignCardToDeckChoice_ =<< genPlayerCard Assets.rubyStandish
            -- TODO: swap a chaos token (remove 1 / add 1) for the remainder of the
            -- campaign, per the Eastern Expedition setup.
            setNextCampaignStep ObsidianCanyons
        pure c
      {- The Sepulchre of the Sleeper's intro decides whether the scenario is played
      at all — two of its three outcomes skip straight to Interlude III — so it runs
      here rather than in the scenario's own PreScenarioSetup. The branch that does
      play it re-enters via @CampaignSpecific "beginSepulchreOfTheSleeper"@ so the
      real scenario start stays in one place, in 'defaultCampaignRunner'.
      -}
      CampaignStep step | step == SepulchreOfTheSleeper -> scope "sepulchreOfTheSleeper" do
        artifacts <- countM getHasRecord rlyehArtifacts
        glyphs <- getTranslatedGlyphCount
        -- "If at least 1 artifact is checked under 'Artifacts Earned,' and at least
        -- 10 glyphs are translated in the glyph record."
        let prepared = artifacts >= 1 && glyphs >= 10
        scope "intro" do
          flavor do
            setTitle "title"
            p "sepulchreOfTheSleeper1"
            p.basic "checkCampaignLog"
            ul do
              li.validate prepared "proceedToSepulchreOfTheSleeper2"
              li.validate (not prepared) "proceedToTheAwakening"
        if prepared then doStep 2 msg else setNextCampaignStep TheAwakening
        pure c
      DoStep 2 (CampaignStep step) | step == SepulchreOfTheSleeper -> scope "sepulchreOfTheSleeper" do
        artifacts <- countM getHasRecord rlyehArtifacts
        glyphs <- getTranslatedGlyphCount
        innerSanctumUnsealed <- getHasRecord TheInnerSanctumWasUnsealed
        -- "If all 5 artifacts are checked under 'Artifacts Earned,' all 26 glyphs are
        -- translated in the glyph record, and the inner sanctum was unsealed."
        let fullyPrepared = artifacts >= length rlyehArtifacts && glyphs >= 26 && innerSanctumUnsealed
        scope "intro" do
          flavor do
            setTitle "title"
            p "sepulchreOfTheSleeper2"
            p.basic "checkCampaignLog"
            ul do
              li.validate fullyPrepared "proceedToSepulchreOfTheSleeper3"
              li.validate (not fullyPrepared) "addZeroToken"
        if fullyPrepared
          then doStep 3 (CampaignStep step)
          else do
            -- "Otherwise, add a 0 token to the chaos bag for the remainder of the
            -- campaign."
            addChaosToken Zero
            setNextCampaignStep TheAwakening
        pure c
      DoStep 3 (CampaignStep step) | step == SepulchreOfTheSleeper -> scope "sepulchreOfTheSleeper" do
        scope "intro" do
          storyWithChooseOneM'
            ( do
                setTitle "title"
                p "sepulchreOfTheSleeper3"
                p.basic "mustDecide"
                ul do
                  li "knowBetter"
                  li "layItToRest"
            )
            do
              labeled' "knowBetter" do
                -- "Each investigator marks 1 progress under their Task."
                eachInvestigator \iid -> do
                  taskKeys <- getInvestigatorTasks iid
                  for_ taskKeys \(key, _, _) -> incrementRecordCountForInvestigator iid key 1
                record TheInvestigatorsDidNotConfrontTheNightmare
                setNextCampaignStep TheAwakening
              labeled' "layItToRest" $ campaignSpecific_ "beginSepulchreOfTheSleeper"
        pure c
      CampaignSpecific "beginSepulchreOfTheSleeper" _ ->
        lift $ defaultCampaignRunner (CampaignStep SepulchreOfTheSleeper) c
      -- Interlude III: The Awakening — the Sleeper rises; both expeditions reunite.
      CampaignStep (InterludeStep 3 _) -> scope "theAwakening" do
        hasArtifact <- anyM getHasRecord rlyehArtifacts
        flavor do
          setTitle "title"
          p "awakening1"
          p.basic "checkCampaignLog"
          ul do
            li.validate hasArtifact "proceedToTheAwakening2"
            li.validate (not hasArtifact) "skipToTheAwakening3"
        if hasArtifact
          then do
            flavor do
              setTitle "title"
              p "awakening2"
              ul $ li "replaceChaosTokensWithTwo"
            removeAllChaosTokens Cultist
            removeAllChaosTokens Tablet
            removeAllChaosTokens ElderThing
            replicateM_ 2 $ addChaosToken Cultist
            replicateM_ 2 $ addChaosToken Tablet
            replicateM_ 2 $ addChaosToken ElderThing
          else do
            flavor do
              setTitle "title"
              p "awakening3"
              ul do
                li "replaceChaosTokens"
                li "proceedToTheAwakening4"
            removeAllChaosTokens Cultist
            removeAllChaosTokens Tablet
            removeAllChaosTokens ElderThing
            addChaosToken Cultist
            addChaosToken Tablet
            replicateM_ 2 $ addChaosToken ElderThing
        flavor do
          setTitle "title"
          p "awakening4"
          p.basic.right "proceedToReturnToArkham"
        setNextCampaignStep ReturnToArkham
        pure c
      -- Interlude IV: Return to Arkham — resolve every investigator's Task.
      CampaignStep (InterludeStep 4 _) -> scope "returnToArkham" do
        hasArtifact <- anyM getHasRecord rlyehArtifacts
        flavor do
          setTitle "title"
          p.basic "checkCampaignLog"
          ul do
            li.validate hasArtifact "proceedToReturnToArkham1"
            li.validate (not hasArtifact) "skipToReturnToArkham2"
        flavor do
          setTitle "title"
          p $ if hasArtifact then "returnToArkham1" else "returnToArkham2"
          ul do
            li.nested "checkTasks" do
              li "gainTaskExperience"
              li "resolveTasks"
        eachInvestigator (`forInvestigator` msg)
        setNextCampaignStep Finale
        pure c
      ForInvestigator iid (CampaignStep (InterludeStep 4 _)) -> scope "returnToArkham" do
        investigatorTasks <- getInvestigatorTasks iid
        for_ investigatorTasks \(task, cardDef, label) -> do
          progress <- getRecordCountForInvestigator iid task
          when (progress > 0) $ gainXp iid CampaignSource (ikey "xp.taskProgress") progress
          let completed = progress >= 5
          scope "tasks" $ scope label do
            -- No Place Like Home's failure is the only Task outcome that asks the
            -- investigator anything. Same entry as every other Task, but its two
            -- trauma choices take the place of the Continue button, so the text is
            -- still above them when the decision is made.
            if completed || task /= NoPlaceLikeHome
              then flavor $ taskStory completed
              else storyWithChooseOneM' (taskStory completed) do
                unscoped $ countVar 1 $ labeled' "sufferPhysicalTrauma" $ sufferPhysicalTrauma iid 1
                unscoped $ countVar 1 $ labeled' "sufferMentalTrauma" $ sufferMentalTrauma iid 1
          if completed
            then do
              for_ (completedTask task) \completedCard -> do
                removeCampaignCardFromDeck iid cardDef
                addCampaignCardToDeck iid DoNotShuffleIn completedCard
              for_ (completedTaskRecord task) $ recordForInvestigator iid
              when (task == DreamsOfDestruction) $ addChaosToken PlusOne
            else do
              removeCampaignCardFromDeck iid cardDef
              case task of
                WalkInFaith -> do
                  sufferMentalTrauma iid 1
                  recordForInvestigator iid LostTheirFaith
                GoodMoney -> do
                  sufferPhysicalTrauma iid 1
                  -- "Search the collection for an Injury or Criminal basic weakness."
                  -- Anything this investigator already holds is out of their own
                  -- collection, and reprints are distinct CardDefs, so both the
                  -- exclusion and the dedupe key off the canonical code -- otherwise
                  -- Stubborn Detective is offered once per printing, and offered at
                  -- all to a player who already has it. Sorting first makes the
                  -- surviving printing the lowest card code (the original) rather
                  -- than whichever happened to come first.
                  taken <- getTakenBasicWeaknesses iid
                  let weaknesses =
                        nubOrdOn (canonicalCardCode . toCardDef)
                          $ sortOn toCardCode
                          $ filter ((`notMember` taken) . canonicalCardCode . toCardDef)
                          $ filter
                            (`cardMatch` (BasicWeaknessCard <> mapOneOf CardWithTrait [Injury, Criminal]))
                          $ map (`lookupPlayerCard` nullCardId)
                          $ toList allPlayerCards
                  unless (null weaknesses)
                    $ chooseOneM iid
                    $ cardsLabeled weaknesses
                    $ addCampaignCardToDeck iid DoNotShuffleIn
                DreamsOfDestruction -> do
                  sufferMentalTrauma iid 1
                  removeChaosToken AutoFail
                -- The trauma choice is part of this Task's story entry above.
                NoPlaceLikeHome -> addChaosToken Cultist
                DoNoHarm -> do
                  sufferMentalTrauma iid 1
                  addChaosToken Tablet
                ToeTheLine -> do
                  sufferPhysicalTrauma iid 1
                  replaceCampaignChaosTokens 2 lowerChaosTokenByTwo \replaced ->
                    scope "tasks" $ scope "toeTheLine" $ scope "tokenReplacement" $ storyBuild do
                      setTitle "title"
                      p "body"
                      for_ replaced (uncurry chaosTokenMorph)
                ProveYourWorth -> sufferMentalTrauma iid 1
                PlumbTheDepths -> do
                  sufferMentalTrauma iid 1
                  addChaosToken Skull
                _ -> pure ()
        pure c
      CampaignStep Finale -> scope "finale" do
        flavor $ setTitle "title" >> p "body"
        -- 'nextStep' routes Finale to The Doom of Arkham, Part I; without this the
        -- campaign has nothing queued once the flavor is dismissed and hangs.
        nextCampaignStep
        pure c
      -- Epilogue. Sepulchre of the Sleeper's Resolution 1 wins the campaign outright
      -- and comes straight here, as do The Doom of Arkham's endings.
      CampaignStep EpilogueStep -> scope "epilogue" do
        annihilatedArkham <- getHasRecord CthulhuAnnihilatedTheCityOfArkham
        drivenAway <- getHasRecord CthulhuWasDrivenAway
        banished <- getHasRecord CthulhuWasBanished
        arkhamDestroyed <- getHasRecord ArkhamWasDestroyed
        haltedAwakening <- getHasRecord TheInvestigatorsHaltedCthulhusAwakening
        -- The Campaign Log entries are checked in printed order; the first that
        -- matches is the epilogue that gets read.
        let epilogue :: Int
            epilogue
              | annihilatedArkham = 1
              | drivenAway || (banished && arkhamDestroyed) = 2
              | banished = 3
              | haltedAwakening = 4
              | otherwise = 0
        flavor do
          setTitle "title"
          p.basic "checkCampaignLog"
          ul do
            li.validate (epilogue == 1) "proceedToEpilogue1"
            li.validate (epilogue == 2) "proceedToEpilogue2"
            li.validate (epilogue == 3) "proceedToEpilogue3"
            li.validate (epilogue == 4) "proceedToEpilogue4"
        when (epilogue > 0) do
          scope ("epilogue" <> tshow epilogue) $ flavor $ setTitle "title" >> p "body"
        gameOver
        pure c
      -- Glyph cards push @campaignSpecific "translateGlyph" ("rune_<letter>", "<word>")@
      -- when translated; record the rune letter into the DiscoveredGlyphs set so the
      -- Campaign Log glyph page (DiscoveredRunes.vue) lights it up.
      CampaignSpecific "translateGlyph" v -> do
        let (glyph, _word) = toResult v :: (Text, Text)
        for_ (glyphLetter glyph) \letter -> recordSetInsert DiscoveredGlyphs [String letter]
        -- Cards that react to glyphs being translated (Careful Navigation) watch this
        -- window. It has to fire after the record above so the reaction's criteria
        -- see the glyph that just arrived.
        checkAfter $ Window.CampaignEvent "translateGlyph" Nothing v
        pure c
      _ -> lift $ defaultCampaignRunner msg c

-- | Extract the uppercase rune letter from a @"rune_<letter>"@ glyph id.
glyphLetter :: Text -> Maybe Text
glyphLetter g = case T.stripPrefix "rune_" g of
  Just s | not (T.null s) -> Just (T.toUpper (T.take 1 s))
  _ -> Nothing
