module Arkham.Campaign.Campaigns.EdgeOfTheEarth (edgeOfTheEarth) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Campaign.Import.Lifted
import Arkham.Campaign.Types (campaignChaosBag, campaignCompletedSteps, campaignStore)
import Arkham.CampaignLog
import Arkham.Campaigns.EdgeOfTheEarth.CampaignSteps
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Campaigns.EdgeOfTheEarth.Key
import Arkham.Campaigns.EdgeOfTheEarth.Supplies
import Arkham.Card
import Arkham.ChaosToken
import Arkham.EncounterSet (EncounterSet (Tekelili))
import Arkham.Event.Cards qualified as Events
import Arkham.FlavorText
import Arkham.Helpers.Campaign (getOwner)
import Arkham.Helpers.Log
import Arkham.Helpers.Query (getInvestigators, getLead)
import Arkham.Helpers.Text
import Arkham.Helpers.Xp (toBonus)
import Arkham.Matcher hiding (AssetDefeated)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Modifier
import Arkham.Projection
import Arkham.Scenarios.IceAndDeath.Helpers
import Arkham.Source
import Arkham.Target
import Data.Map.Strict qualified as Map

newtype EdgeOfTheEarth = EdgeOfTheEarth CampaignAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor)

edgeOfTheEarth :: Difficulty -> EdgeOfTheEarth
edgeOfTheEarth = campaign EdgeOfTheEarth (CampaignId "08") "Edge of the Earth"

instance IsCampaign EdgeOfTheEarth where
  campaignTokens = chaosBagContents
  nextStep a = case (toAttrs a).normalizedStep of
    PrologueStep -> continue IceAndDeathPart1
    IceAndDeathPart1 -> continue $ CheckpointStep 1
    IceAndDeathPart2 -> continue $ CheckpointStep 2
    IceAndDeathPart3 -> continue $ InterludeStep 1 Nothing
    FatalMirage ->
      if
        | CityOfTheElderThings `elem` campaignCompletedSteps (toAttrs a) ->
            continue TheHeartOfMadnessPart1
        | ToTheForbiddenPeaks `elem` campaignCompletedSteps (toAttrs a) ->
            continue CityOfTheElderThings
        | otherwise -> continue ToTheForbiddenPeaks
    ToTheForbiddenPeaks -> continue $ InterludeStep 2 Nothing
    CityOfTheElderThings -> continue $ InterludeStep 3 Nothing
    TheHeartOfMadnessPart1 -> continue $ CheckpointStep 3
    TheHeartOfMadnessPart2 -> continue EpilogueStep
    EpilogueStep -> Nothing
    other -> defaultNextStep other

instance RunMessage EdgeOfTheEarth where
  runMessage msg c@(EdgeOfTheEarth attrs) = runQueueT $ campaignI18n $ case msg of
    StartCampaign -> do
      let addPartner partner = logL . partnersL . at partner.cardCode ?~ CampaignLogPartner 0 0 Safe
      lift
        $ defaultCampaignRunner msg
        $ EdgeOfTheEarth
        $ foldl' (flip addPartner) attrs expeditionTeam
    CampaignStep PrologueStep -> do
      story $ i18nWithTitle "madnessUnderTheIce"
      story $ i18nWithTitle "prologue"
      storyWithChooseOneM (i18nWithTitle "prologue1") do
        labeled' "prologue1Believe"
          $ doStep 2
          $ CampaignStep PrologueStep
        labeled' "prologue1Wild"
          $ doStep 3
          $ CampaignStep PrologueStep
      pure c
    DoStep 2 (CampaignStep PrologueStep) -> do
      storyWithContinue (i18nWithTitle "prologue2") "Skip to _Prologue 4_"
      record TheInvestigatorsConvincedDyerToAllowTheExpedition
      addChaosToken Cultist
      doStep 4 $ CampaignStep PrologueStep
      pure c
    DoStep 3 (CampaignStep PrologueStep) -> do
      storyWithContinue (i18nWithTitle "prologue3") "Proceed to _Prologue 4_"
      record TheInvestigatorsDidNotBelieveDyersReport
      addChaosToken Tablet
      doStep 4 $ CampaignStep PrologueStep
      pure c
    DoStep 4 (CampaignStep PrologueStep) -> do
      storyWithChooseOneM (i18nWithTitle "prologue4") do
        labeled' "readPartnerIntros" do
          storyWithCard Assets.drAmyKenslerProfessorOfBiology $ i18n "amyKensler"
          storyWithCard Assets.roaldEllsworthIntrepidExplorer $ i18n "roaldEllsworth"
          storyWithCard Assets.jamesCookieFredericksDubiousChoice $ i18n "jamesFredericks"
          storyWithCard Assets.takadaHirokoAeroplaneMechanic $ i18n "takadaHiroko"
          storyWithCard Assets.averyClaypoolAntarcticGuide $ i18n "averyClaypool"
          storyWithCard Assets.drMalaSinhaDaringPhysician $ i18n "malaSinha"
          storyWithCard Assets.eliyahAshevakDogHandler $ i18n "eliyahAshevak"
          storyWithCards [Assets.professorWilliamDyerProfessorOfGeology, Assets.danforthBrilliantStudent]
            $ i18n "williamDyer"
        labeled' "skipPartnerIntros" nothing
      nextCampaignStep
      pure c
    CampaignStep (CheckpointStep 1) -> scope "checkpoint1" do
      sv <- fromJustNote "missing shelter" <$> getCurrentShelterValue
      mia <- drop sv <$> (shuffle =<< getRemainingPartners)
      if null mia
        then do
          story $ i18nWithTitle "theDisappearance1"
          doStep 2 msg
        else do
          storyWithChooseOneM (i18nWithTitle "theDisappearance1") $ campaignI18n do
            labeled' "theyreOnTheirOwn" do
              for_ mia \partner -> do
                push $ SetPartnerStatus partner.cardCode Eliminated
              doStep 2 msg
            labeled' "goAfterMissingTeamMembers" do
              for_ mia \partner -> do
                push $ SetPartnerStatus partner.cardCode Mia
              doStep 3 msg
      pure c
    DoStep 2 (CampaignStep (CheckpointStep 1)) -> scope "checkpoint1" do
      story $ i18nWithTitle "theDisappearance2"
      push $ NextCampaignStep (continue $ CheckpointStep 2)
      pure c
    DoStep 3 (CampaignStep (CheckpointStep 1)) -> scope "checkpoint1" do
      story $ i18nWithTitle "theDisappearance3"
      push $ NextCampaignStep (continue IceAndDeathPart2)
      pure c
    CampaignStep (CheckpointStep 2) -> scope "checkpoint2" do
      storyWithChooseOneM (i18nWithTitle "theAttack1") $ campaignI18n do
        labeled' "runForYourLives" $ doStep 2 msg
        labeled' "standAndFight" $ doStep 3 msg
      pure c
    DoStep 2 (CampaignStep (CheckpointStep 2)) -> scope "checkpoint2" do
      story $ i18nWithTitle "theAttack2"
      record TheTeamFledToTheMountains
      push $ NextCampaignStep $ continue $ InterludeStep 1 Nothing
      pure c
    DoStep 3 (CampaignStep (CheckpointStep 2)) -> scope "checkpoint2" do
      story $ i18nWithTitle "theAttack3"
      push $ NextCampaignStep $ continue IceAndDeathPart3
      pure c
    CampaignStep (InterludeStep 1 _) -> scope "interlude1" do
      story $ i18nWithTitle "restfulNight1"
      fledToTheMountains <- getHasRecord TheTeamFledToTheMountains
      doStep (if fledToTheMountains then 4 else 3) msg
      push $ CampaignStep $ InterludeStepPart 1 Nothing 2
      pure c
    CampaignStep (InterludeStepPart 1 _ 2) -> scope "interlude1" do
      eliminated <- getPartnersWithStatus (== Eliminated)
      let shouldRestlessNight3 = length eliminated >= 3
      story
        $ withVars
          [ "restfulNight3Status" .= String (if shouldRestlessNight3 then "valid" else "invalid")
          , "forbiddenPeaksStatus" .= String (if not shouldRestlessNight3 then "valid" else "invalid")
          ]
        $ i18nWithTitle "restfulNight2"
      if shouldRestlessNight3
        then push $ CampaignStep (InterludeStepPart 1 Nothing 3)
        else push $ NextCampaignStep $ continue ToTheForbiddenPeaks
      pure c
    CampaignStep (InterludeStepPart 1 _ 3) -> scope "interlude1" do
      storyWithChooseOneM (i18nWithTitle "restfulNight3") $ campaignI18n do
        labeled' "openTheDoorAndVenture" do
          push $ NextCampaignStep $ continue FatalMirage
        labeled' "ignoreTheDoor" do
          push $ NextCampaignStep $ continue ToTheForbiddenPeaks
      pure c
    DoStep n msg'@(CampaignStep (InterludeStep 1 _)) | n > 0 -> scope "interlude1" do
      let choices :: [CardCode] =
            toResult
              $ Map.findWithDefault
                (toJSON $ map toCardCode $ toList expeditionTeam)
                "interlude1"
                (campaignStore attrs)
      when (notNull choices) do
        lead <- getLead
        remainingPartners <- map toPartnerCode <$> getRemainingPartners
        let choiceMade choice = push $ SetGlobal CampaignTarget "interlude1" (toJSON $ filter (/= choice) choices)
        chooseOneM lead $ campaignI18n do
          countVar n $ questionLabeled' "youCanStillCheckTeamMembers"
          let dyer = Assets.professorWilliamDyerProfessorOfGeology.cardCode
          when (dyer `elem` choices) do
            labeled' "williamDyer" do
              choiceMade dyer
              let alive = dyer `elem` remainingPartners
              blueStory
                $ compose
                  [ validateEntry alive "williamDyerIsAlive"
                  , hr
                  , validateEntry (not alive) "williamDyerIsCrossedOut"
                  ]
              if alive
                then do
                  iids <- select $ DeckWith $ HasCard $ CardFromEncounterSet Tekelili
                  if null iids
                    then doStep n msg'
                    else do
                      chooseOneM lead $ campaignI18n $ for_ iids \iid -> do
                        questionLabeled' "removeTekeliliWeaknesses"
                        portraitLabeled iid do
                          cards <- select $ inDeckOf iid <> basic (CardFromEncounterSet Tekelili)
                          chooseUpToNM' iid 5 "doNotRemoveAnymore" do
                            for_ cards \card -> cardLabeled card $ removeCardFromDeckForCampaign iid card

                      doStep (n - 1) msg'
                else do
                  iids <- getInvestigators
                  addCampaignCardToDeckChoice iids DoNotShuffleIn Events.dyersSketches
                  doStep (n - 1) msg'

          let danforth = Assets.danforthBrilliantStudent.cardCode
          when (danforth `elem` choices) do
            labeled' "danforth" do
              choiceMade danforth
              let alive = danforth `elem` remainingPartners
              blueStory
                $ compose
                  [ validateEntry alive "danforthIsAlive"
                  , hr
                  , validateEntry (not alive) "danforthIsCrossedOut"
                  ]
              if alive
                then do
                  iids <- getInvestigators
                  chooseOneM lead $ campaignI18n $ for_ iids \iid -> do
                    questionLabeled' "beginScenarioIIWithExtraCards"
                    portraitLabeled iid do
                      scenarioSetupModifier "08596" CampaignSource iid (StartingHand 2)
                else do
                  iids <- getInvestigators
                  addCampaignCardToDeckChoice iids DoNotShuffleIn Assets.collectedWorksOfPoe
              doStep (n - 1) msg'

          let kensler = Assets.drAmyKenslerProfessorOfBiology.cardCode
          when (kensler `elem` choices) do
            labeled' "drAmyKensler" do
              choiceMade kensler
              let alive = kensler `elem` remainingPartners
              blueStory
                $ compose
                  [ validateEntry alive "kenslerIsAlive"
                  , hr
                  , validateEntry (not alive) "kenslerIsCrossedOut"
                  ]
              if alive
                then record DrKenslerIsSharingHerResearchWithYou
                else do
                  iids <- getInvestigators
                  addCampaignCardToDeckChoice iids DoNotShuffleIn Assets.kenslersLog
              doStep (n - 1) msg'

          let sinha = Assets.drMalaSinhaDaringPhysician.cardCode
          when (sinha `elem` choices) do
            labeled' "drMalaSinha" do
              choiceMade sinha
              let alive = sinha `elem` remainingPartners
              blueStory
                $ compose
                  [ validateEntry alive "sinhaIsAlive"
                  , hr
                  , validateEntry (not alive) "sinhaIsCrossedOut"
                  ]
              if alive
                then do
                  injured <- select InvestigatorWithPhysicalTrauma
                  damagedPartners <- filter ((> 0) . (.damage)) <$> getRemainingPartners
                  if null injured && null damagedPartners
                    then doStep n msg'
                    else do
                      chooseOneM lead $ campaignI18n do
                        labeled' "doNotPerformHealing" nothing
                        for_ injured \iid ->
                          portraitLabeled iid $ push $ HealTrauma iid 1 0
                        for_ damagedPartners \partner -> do
                          cardLabeled partner $ push $ HealDamage (CardCodeTarget partner.cardCode) CampaignSource 1
                      doStep (n - 1) msg'
                else do
                  iids <- getInvestigators
                  addCampaignCardToDeckChoice iids DoNotShuffleIn Assets.sinhasMedicalKit
                  doStep (n - 1) msg'

          let cookie = Assets.jamesCookieFredericksDubiousChoice.cardCode
          when (cookie `elem` choices) do
            labeled' "jamesCookieFredericks" do
              choiceMade cookie
              let alive = cookie `elem` remainingPartners
              blueStory
                $ compose
                  [ validateEntry alive "cookieIsAlive"
                  , hr
                  , validateEntry (not alive) "cookieIsCrossedOut"
                  ]
              if alive
                then do
                  iids <- getInvestigators
                  chooseOneM lead $ campaignI18n $ for_ iids \iid -> do
                    questionLabeled' "earnsBonusExperience"
                    portraitLabeled iid do
                      interludeXp iid $ toBonus "cookiesAdvice" 1
                else do
                  iids <- getInvestigators
                  addCampaignCardToDeckChoice iids DoNotShuffleIn Assets.cookiesCustom32
              doStep (n - 1) msg'

          let ellsworth = Assets.roaldEllsworthIntrepidExplorer.cardCode
          let ellsworthAlive = ellsworth `elem` remainingPartners

          let claypool = Assets.averyClaypoolAntarcticGuide.cardCode
          when (claypool `elem` choices) do
            labeled' "averyClaypool" do
              choiceMade claypool
              let alive = claypool `elem` remainingPartners
              blueStory
                $ compose
                  [ validateEntry alive "claypoolIsAlive"
                  , validateEntry (alive && ellsworthAlive) "claypoolIsAliveEllsworth"
                  , hr
                  , validateEntry (not alive) "claypoolIsCrossedOut"
                  ]
              if alive
                then do
                  if #frost `elem` campaignChaosBag attrs
                    then do
                      push $ RemoveChaosToken #frost
                      doStep (n - 1) msg'
                    else doStep n msg'
                else do
                  iids <- getInvestigators
                  addCampaignCardToDeckChoice iids DoNotShuffleIn Assets.claypoolsFurs
                  doStep (n - 1) msg'

          when (ellsworth `elem` choices) do
            labeled' "roaldEllsworth" do
              choiceMade ellsworth
              blueStory
                $ compose
                  [ validateEntry ellsworthAlive "ellsworthIsAlive"
                  , hr
                  , validateEntry (not ellsworthAlive) "ellsworthIsCrossedOut"
                  ]
              if ellsworthAlive
                then record TheInvestigatorsScoutedTheMountainPass
                else do
                  iids <- getInvestigators
                  addCampaignCardToDeckChoice iids DoNotShuffleIn Assets.ellsworthsBoots
              doStep (n - 1) msg'

          let takada = Assets.takadaHirokoAeroplaneMechanic.cardCode
          when (takada `elem` choices) do
            labeled' "takadaHiroko" do
              choiceMade takada
              let alive = takada `elem` remainingPartners
              blueStory
                $ compose
                  [ validateEntry alive "takadaIsAlive"
                  , hr
                  , validateEntry (not alive) "takadaIsCrossedOut"
                  ]
              if alive
                then do
                  iids <- getInvestigators
                  chooseOneM lead $ campaignI18n $ for_ iids \iid -> do
                    questionLabeled' "beginScenarioIIWithExtraResources"
                    portraitLabeled iid do
                      scenarioSetupModifier "08596" CampaignSource iid (StartingResources 3)
                else do
                  iids <- getInvestigators
                  addCampaignCardToDeckChoice iids DoNotShuffleIn Events.takadasCache
              doStep (n - 1) msg'

          let ashevak = Assets.eliyahAshevakDogHandler.cardCode
          when (ashevak `elem` choices) do
            labeled' "eliyahAshevak" do
              choiceMade ashevak
              let alive = ashevak `elem` remainingPartners
              blueStory
                $ compose
                  [ validateEntry alive "ashevakIsAlive"
                  , hr
                  , validateEntry (not alive) "ashevakIsCrossedOut"
                  ]
              if alive
                then do
                  injured <- select InvestigatorWithMentalTrauma
                  damagedPartners <- filter ((> 0) . (.horror)) <$> getRemainingPartners
                  if null injured && null damagedPartners
                    then doStep n msg'
                    else do
                      chooseOneM lead $ campaignI18n do
                        labeled' "doNotPerformHealing" nothing
                        for_ injured \iid ->
                          portraitLabeled iid $ push $ HealTrauma iid 0 1
                        for_ damagedPartners \partner -> do
                          cardLabeled partner $ push $ HealHorror (CardCodeTarget partner.cardCode) CampaignSource 1
                      doStep (n - 1) msg'
                else do
                  iids <- getInvestigators
                  addCampaignCardToDeckChoice iids DoNotShuffleIn Assets.anyuFaithfulCompanion
                  doStep (n - 1) msg'

      pure c
    CampaignStep (InterludeStep 2 _) -> scope "interlude2" do
      story $ i18nWithTitle "endlessNight1"
      foundAnotherWayThroughTheMountains <- getHasRecord TheTeamFoundAnotherWayThroughTheMountains
      doStep (if foundAnotherWayThroughTheMountains then 4 else 3) msg
      push $ CampaignStep $ InterludeStepPart 2 Nothing 2
      pure c
    DoStep n msg'@(CampaignStep (InterludeStep 2 _)) | n > 0 -> scope "interlude2" do
      let choices :: [CardCode] =
            toResult
              $ Map.findWithDefault
                (toJSON $ map toCardCode $ toList expeditionTeam)
                "interlude2"
                (campaignStore attrs)
      when (notNull choices) do
        lead <- getLead
        remainingPartners <- map toPartnerCode <$> getRemainingPartners
        let choiceMade choice = push $ SetGlobal CampaignTarget "interlude2" (toJSON $ filter (/= choice) choices)
        chooseOneM lead $ campaignI18n do
          countVar n $ questionLabeled' "youCanStillCheckTeamMembers"
          let dyer = Assets.professorWilliamDyerProfessorOfGeology.cardCode
          when (dyer `elem` choices) do
            labeled' "williamDyer" do
              choiceMade dyer
              let alive = dyer `elem` remainingPartners
              owned <- isJust <$> getOwner Events.dyersSketches
              blueStory
                $ compose
                  [ validateEntry alive "williamDyerIsAlive"
                  , hr
                  , validateEntry (not alive && not owned) "williamDyerIsCrossedOut"
                  ]
              if
                | alive -> do
                    iids <- select $ DeckWith $ HasCard $ CardFromEncounterSet Tekelili
                    if null iids
                      then doStep n msg'
                      else do
                        chooseOneM lead $ campaignI18n $ for_ iids \iid -> do
                          questionLabeled' "removeTekeliliWeaknesses"
                          portraitLabeled iid do
                            cards <- select $ inDeckOf iid <> basic (CardFromEncounterSet Tekelili)
                            chooseUpToNM' iid 5 "doNotRemoveAnymore" do
                              for_ cards \card -> cardLabeled card $ removeCardFromDeckForCampaign iid card

                        doStep (n - 1) msg'
                | not owned -> do
                    iids <- getInvestigators
                    addCampaignCardToDeckChoice iids DoNotShuffleIn Events.dyersSketches
                    doStep (n - 1) msg'
                | otherwise -> doStep n msg'

          let danforth = Assets.danforthBrilliantStudent.cardCode
          when (danforth `elem` choices) do
            labeled' "danforth" do
              choiceMade danforth
              let alive = danforth `elem` remainingPartners
              owned <- isJust <$> getOwner Assets.collectedWorksOfPoe
              blueStory
                $ compose
                  [ validateEntry alive "danforthIsAlive"
                  , hr
                  , validateEntry (not alive && not owned) "danforthIsCrossedOut"
                  ]
              if
                | alive -> do
                    iids <- getInvestigators
                    chooseOneM lead $ campaignI18n $ for_ iids \iid -> do
                      questionLabeled' "beginScenarioIIIWithExtraCards"
                      portraitLabeled iid do
                        scenarioSetupModifier "08621" CampaignSource iid (StartingHand 2)
                    doStep (n - 1) msg'
                | not owned -> do
                    iids <- getInvestigators
                    addCampaignCardToDeckChoice iids DoNotShuffleIn Assets.collectedWorksOfPoe
                    doStep (n - 1) msg'
                | otherwise -> doStep n msg'

          let kensler = Assets.drAmyKenslerProfessorOfBiology.cardCode
          when (kensler `elem` choices) do
            labeled' "drAmyKensler" do
              choiceMade kensler
              let alive = kensler `elem` remainingPartners
              owned <- isJust <$> getOwner Assets.kenslersLog
              sharingResearch <- getHasRecord DrKenslerIsSharingHerResearchWithYou
              blueStory
                $ compose
                  [ validateEntry (alive && sharingResearch) "kenslerIsAliveAndSharingResearch"
                  , hr
                  , validateEntry (alive && not sharingResearch) "kenslerIsAlive"
                  , hr
                  , validateEntry (not alive && not owned) "kenslerIsCrossedOut"
                  ]
              if
                | alive && sharingResearch -> do
                    record DrKenslerIsOnTheVergeOfUnderstanding
                    doStep (n - 1) msg'
                | not alive && not owned -> do
                    iids <- getInvestigators
                    addCampaignCardToDeckChoice iids DoNotShuffleIn Assets.kenslersLog
                    doStep (n - 1) msg'
                | otherwise -> doStep n msg'

          let sinha = Assets.drMalaSinhaDaringPhysician.cardCode
          when (sinha `elem` choices) do
            labeled' "drMalaSinha" do
              choiceMade sinha
              let alive = sinha `elem` remainingPartners
              owned <- isJust <$> getOwner Assets.sinhasMedicalKit
              blueStory
                $ compose
                  [ validateEntry alive "sinhaIsAlive"
                  , hr
                  , validateEntry (not alive && not owned) "sinhaIsCrossedOut"
                  ]
              if
                | alive -> do
                    injured <- select InvestigatorWithPhysicalTrauma
                    damagedPartners <- filter ((> 0) . (.damage)) <$> getRemainingPartners
                    if null injured && null damagedPartners
                      then doStep n msg'
                      else do
                        chooseOneM lead $ campaignI18n do
                          labeled' "doNotPerformHealing" nothing
                          for_ injured \iid ->
                            portraitLabeled iid $ push $ HealTrauma iid 1 0
                          for_ damagedPartners \partner -> do
                            cardLabeled partner $ push $ HealDamage (CardCodeTarget partner.cardCode) CampaignSource 1
                        doStep (n - 1) msg'
                | not owned -> do
                    iids <- getInvestigators
                    addCampaignCardToDeckChoice iids DoNotShuffleIn Assets.sinhasMedicalKit
                    doStep (n - 1) msg'
                | otherwise -> doStep n msg'

          let cookie = Assets.jamesCookieFredericksDubiousChoice.cardCode
          when (cookie `elem` choices) do
            labeled' "jamesCookieFredericks" do
              choiceMade cookie
              let alive = cookie `elem` remainingPartners
              owned <- isJust <$> getOwner Assets.cookiesCustom32
              blueStory
                $ compose
                  [ validateEntry alive "cookieIsAlive"
                  , hr
                  , validateEntry (not alive && not owned) "cookieIsCrossedOut"
                  ]
              if
                | alive -> do
                    iids <- getInvestigators
                    chooseOneM lead $ campaignI18n $ for_ iids \iid -> do
                      questionLabeled' "earnsBonusExperience"
                      portraitLabeled iid do
                        interludeXp iid $ toBonus "cookiesAdvice" 1
                    doStep (n - 1) msg'
                | not owned -> do
                    iids <- getInvestigators
                    addCampaignCardToDeckChoice iids DoNotShuffleIn Assets.cookiesCustom32
                    doStep (n - 1) msg'
                | otherwise -> doStep n msg'

          let ellsworth = Assets.roaldEllsworthIntrepidExplorer.cardCode

          let claypool = Assets.averyClaypoolAntarcticGuide.cardCode
          let claypoolAlive = claypool `elem` remainingPartners
          when (claypool `elem` choices) do
            labeled' "averyClaypool" do
              choiceMade claypool
              owned <- isJust <$> getOwner Assets.claypoolsFurs
              blueStory
                $ compose
                  [ validateEntry claypoolAlive "claypoolIsAlive"
                  , hr
                  , validateEntry (not claypoolAlive) "claypoolIsCrossedOut"
                  ]
              if
                | claypoolAlive ->
                    if #frost `elem` campaignChaosBag attrs
                      then do
                        push $ RemoveChaosToken #frost
                        doStep (n - 1) msg'
                      else doStep n msg'
                | not owned -> do
                    iids <- getInvestigators
                    addCampaignCardToDeckChoice iids DoNotShuffleIn Assets.claypoolsFurs
                    doStep (n - 1) msg'
                | otherwise -> doStep n msg'

          when (ellsworth `elem` choices) do
            labeled' "roaldEllsworth" do
              choiceMade ellsworth
              let alive = ellsworth `elem` remainingPartners
              owned <- isJust <$> getOwner Assets.ellsworthsBoots
              blueStory
                $ compose
                  [ validateEntry alive "ellsworthIsAlive"
                  , validateEntry (alive && claypoolAlive) "ellsworthIsAliveClaypool"
                  , hr
                  , validateEntry (not alive && not owned) "ellsworthIsCrossedOut"
                  ]
              if
                | alive -> do
                    record TheInvestigatorsScoutedTheCityOutskirts
                    doStep (n - 1) msg'
                | not owned -> do
                    iids <- getInvestigators
                    addCampaignCardToDeckChoice iids DoNotShuffleIn Assets.ellsworthsBoots
                    doStep (n - 1) msg'
                | otherwise -> doStep n msg'

          let takada = Assets.takadaHirokoAeroplaneMechanic.cardCode
          when (takada `elem` choices) do
            labeled' "takadaHiroko" do
              choiceMade takada
              let alive = takada `elem` remainingPartners
              owned <- isJust <$> getOwner Events.takadasCache
              blueStory
                $ compose
                  [ validateEntry alive "takadaIsAlive"
                  , hr
                  , validateEntry (not alive && not owned) "takadaIsCrossedOut"
                  ]
              if
                | alive -> do
                    iids <- getInvestigators
                    chooseOneM lead $ campaignI18n $ for_ iids \iid -> do
                      questionLabeled' "beginScenarioIIIWithExtraResources"
                      portraitLabeled iid do
                        scenarioSetupModifier "08621" CampaignSource iid (StartingResources 3)
                | not owned -> do
                    iids <- getInvestigators
                    addCampaignCardToDeckChoice iids DoNotShuffleIn Events.takadasCache
                    doStep (n - 1) msg'
                | otherwise -> doStep n msg'

          let ashevak = Assets.eliyahAshevakDogHandler.cardCode
          when (ashevak `elem` choices) do
            labeled' "eliyahAshevak" do
              choiceMade ashevak
              let alive = ashevak `elem` remainingPartners
              owned <- isJust <$> getOwner Assets.anyuFaithfulCompanion
              blueStory
                $ compose
                  [ validateEntry alive "ashevakIsAlive"
                  , hr
                  , validateEntry (not alive && not owned) "ashevakIsCrossedOut"
                  ]
              if
                | alive -> do
                    injured <- select InvestigatorWithMentalTrauma
                    damagedPartners <- filter ((> 0) . (.horror)) <$> getRemainingPartners
                    if null injured && null damagedPartners
                      then doStep n msg'
                      else do
                        chooseOneM lead $ campaignI18n do
                          labeled' "doNotPerformHealing" nothing
                          for_ injured \iid -> portraitLabeled iid $ push $ HealTrauma iid 0 1
                          for_ damagedPartners \partner -> do
                            cardLabeled partner $ push $ HealHorror (CardCodeTarget partner.cardCode) CampaignSource 1
                        doStep (n - 1) msg'
                | not owned -> do
                    iids <- getInvestigators
                    addCampaignCardToDeckChoice iids DoNotShuffleIn Assets.anyuFaithfulCompanion
                    doStep (n - 1) msg'
                | otherwise -> doStep n msg'
      pure c
    CampaignStep (InterludeStepPart 2 _ 2) -> scope "interlude2" do
      let shouldEndlessNight3 = FatalMirage `elem` campaignCompletedSteps attrs
      eliminated <- getPartnersWithStatus (== Eliminated)
      let shouldEndlessNight4 = not shouldEndlessNight3 && length eliminated >= 3
      story
        $ withVars
          [ "endlessNight3Status" .= String (if shouldEndlessNight3 then "valid" else "invalid")
          , "endlessNight4Status" .= String (if shouldEndlessNight4 then "valid" else "invalid")
          , "cityOfTheElderThingsStatus"
              .= String (if not (shouldEndlessNight3 || shouldEndlessNight4) then "valid" else "invalid")
          ]
        $ i18nWithTitle "endlessNight2"
      if
        | shouldEndlessNight3 -> push $ CampaignStep (InterludeStepPart 2 Nothing 3)
        | shouldEndlessNight4 -> push $ CampaignStep (InterludeStepPart 2 Nothing 4)
        | otherwise -> push $ NextCampaignStep $ continue CityOfTheElderThings
      pure c
    CampaignStep (InterludeStepPart 2 _ 3) -> scope "interlude2" do
      storyWithChooseOneM (i18nWithTitle "endlessNight3") $ campaignI18n do
        labeled' "openTheDoorAndVentureOnceMore" do
          push $ NextCampaignStep $ continue FatalMirage
        labeled' "ignoreTheDoor" do
          push $ NextCampaignStep $ continue CityOfTheElderThings
      pure c
    CampaignStep (InterludeStepPart 2 _ 4) -> scope "interlude2" do
      storyWithChooseOneM (i18nWithTitle "endlessNight4") $ campaignI18n do
        labeled' "openTheDoorAndVenture" do
          push $ NextCampaignStep $ continue FatalMirage
        labeled' "ignoreTheDoor" do
          push $ NextCampaignStep $ continue CityOfTheElderThings
      pure c
    CampaignStep (InterludeStep 3 _) -> scope "interlude3" do
      story $ i18nWithTitle "finalNight1"
      guidedToTheTunnel <- getHasRecord TheTeamWasGuidedToTheHiddenTunnel
      doStep (if guidedToTheTunnel then 4 else 3) msg
      push $ CampaignStep $ InterludeStepPart 3 Nothing 2
      pure c
    DoStep n msg'@(CampaignStep (InterludeStep 3 _)) | n > 0 -> scope "interlude3" do
      let choices :: [CardCode] =
            toResult
              $ Map.findWithDefault
                (toJSON $ map toCardCode $ toList expeditionTeam)
                "interlude3"
                (campaignStore attrs)
      when (notNull choices) do
        lead <- getLead
        remainingPartners <- map toPartnerCode <$> getRemainingPartners
        let choiceMade choice = push $ SetGlobal CampaignTarget "interlude3" (toJSON $ filter (/= choice) choices)
        chooseOneM lead $ campaignI18n do
          countVar n $ questionLabeled' "youCanStillCheckTeamMembers"
          let dyer = Assets.professorWilliamDyerProfessorOfGeology.cardCode
          when (dyer `elem` choices) do
            labeled' "williamDyer" do
              choiceMade dyer
              let alive = dyer `elem` remainingPartners
              owned <- isJust <$> getOwner Events.dyersSketches
              blueStory
                $ compose
                  [ validateEntry alive "williamDyerIsAlive"
                  , hr
                  , validateEntry (not alive && not owned) "williamDyerIsCrossedOut"
                  ]
              if
                | alive -> do
                    iids <- select $ DeckWith $ HasCard $ CardFromEncounterSet Tekelili
                    if null iids
                      then doStep n msg'
                      else do
                        chooseOneM lead $ campaignI18n $ for_ iids \iid -> do
                          questionLabeled' "removeTekeliliWeaknesses"
                          portraitLabeled iid do
                            cards <- select $ inDeckOf iid <> basic (CardFromEncounterSet Tekelili)
                            chooseUpToNM' iid 5 "doNotRemoveAnymore" do
                              for_ cards \card -> cardLabeled card $ removeCardFromDeckForCampaign iid card

                        doStep (n - 1) msg'
                | not owned -> do
                    iids <- getInvestigators
                    addCampaignCardToDeckChoice iids DoNotShuffleIn Events.dyersSketches
                    doStep (n - 1) msg'
                | otherwise -> doStep n msg'

          let danforth = Assets.danforthBrilliantStudent.cardCode
          when (danforth `elem` choices) do
            labeled' "danforth" do
              choiceMade danforth
              let alive = danforth `elem` remainingPartners
              owned <- isJust <$> getOwner Assets.collectedWorksOfPoe
              blueStory
                $ compose
                  [ validateEntry alive "danforthIsAlive"
                  , hr
                  , validateEntry (not alive && not owned) "danforthIsCrossedOut"
                  ]
              if
                | alive -> do
                    iids <- getInvestigators
                    chooseOneM lead $ campaignI18n $ for_ iids \iid -> do
                      questionLabeled' "beginScenarioIVWithExtraCards"
                      portraitLabeled iid
                        $ push
                        $ SetGlobal CampaignTarget "heartOfMadnessExtraCards" (toJSON [iid])
                    doStep (n - 1) msg'
                | not owned -> do
                    iids <- getInvestigators
                    addCampaignCardToDeckChoice iids DoNotShuffleIn Assets.collectedWorksOfPoe
                    doStep (n - 1) msg'
                | otherwise -> doStep n msg'

          let kensler = Assets.drAmyKenslerProfessorOfBiology.cardCode
          when (kensler `elem` choices) do
            labeled' "drAmyKensler" do
              choiceMade kensler
              let alive = kensler `elem` remainingPartners
              owned <- isJust <$> getOwner Assets.kenslersLog
              onVerge <- getHasRecord DrKenslerIsOnTheVergeOfUnderstanding
              blueStory
                $ compose
                  [ validateEntry (alive && onVerge) "kenslerIsAliveAndOnTheVergeOfUnderstanding"
                  , hr
                  , validateEntry (alive && not onVerge) "kenslerIsAlive"
                  , hr
                  , validateEntry (not alive && not owned) "kenslerIsCrossedOut"
                  ]
              if
                | alive && onVerge -> do
                    record DrKenslerUnderstandsTheTrueNatureOfTheMiasma
                    doStep (n - 1) msg'
                | not alive && not owned -> do
                    iids <- getInvestigators
                    addCampaignCardToDeckChoice iids DoNotShuffleIn Assets.kenslersLog
                    doStep (n - 1) msg'
                | otherwise -> doStep n msg'

          let sinha = Assets.drMalaSinhaDaringPhysician.cardCode
          when (sinha `elem` choices) do
            labeled' "drMalaSinha" do
              choiceMade sinha
              let alive = sinha `elem` remainingPartners
              owned <- isJust <$> getOwner Assets.sinhasMedicalKit
              blueStory
                $ compose
                  [ validateEntry alive "sinhaIsAlive"
                  , hr
                  , validateEntry (not alive && not owned) "sinhaIsCrossedOut"
                  ]
              if
                | alive -> do
                    injured <- select InvestigatorWithPhysicalTrauma
                    damagedPartners <- filter ((> 0) . (.damage)) <$> getRemainingPartners
                    if null injured && null damagedPartners
                      then doStep n msg'
                      else do
                        chooseOneM lead $ campaignI18n do
                          labeled' "doNotPerformHealing" nothing
                          for_ injured \iid ->
                            portraitLabeled iid $ push $ HealTrauma iid 1 0
                          for_ damagedPartners \partner -> do
                            cardLabeled partner $ push $ HealDamage (CardCodeTarget partner.cardCode) CampaignSource 1
                        doStep (n - 1) msg'
                | not owned -> do
                    iids <- getInvestigators
                    addCampaignCardToDeckChoice iids DoNotShuffleIn Assets.sinhasMedicalKit
                    doStep (n - 1) msg'
                | otherwise -> doStep n msg'

          let cookie = Assets.jamesCookieFredericksDubiousChoice.cardCode
          when (cookie `elem` choices) do
            labeled' "jamesCookieFredericks" do
              choiceMade cookie
              let alive = cookie `elem` remainingPartners
              owned <- isJust <$> getOwner Assets.cookiesCustom32
              blueStory
                $ compose
                  [ validateEntry alive "cookieIsAlive"
                  , hr
                  , validateEntry (not alive && not owned) "cookieIsCrossedOut"
                  ]
              if
                | alive -> do
                    iids <- getInvestigators
                    chooseOneM lead $ campaignI18n $ for_ iids \iid -> do
                      questionLabeled' "earnsBonusExperience"
                      portraitLabeled iid do
                        interludeXp iid $ toBonus "cookiesAdvice" 1
                    doStep (n - 1) msg'
                | not owned -> do
                    iids <- getInvestigators
                    addCampaignCardToDeckChoice iids DoNotShuffleIn Assets.cookiesCustom32
                    doStep (n - 1) msg'
                | otherwise -> doStep n msg'

          let ellsworth = Assets.roaldEllsworthIntrepidExplorer.cardCode

          let claypool = Assets.averyClaypoolAntarcticGuide.cardCode
          when (claypool `elem` choices) do
            labeled' "averyClaypool" do
              choiceMade claypool
              let alive = claypool `elem` remainingPartners
              owned <- isJust <$> getOwner Assets.claypoolsFurs
              blueStory
                $ compose
                  [ validateEntry alive "claypoolIsAlive"
                  , hr
                  , validateEntry (not alive) "claypoolIsCrossedOut"
                  ]
              if
                | alive ->
                    if #frost `elem` campaignChaosBag attrs
                      then do
                        push $ RemoveChaosToken #frost
                        doStep (n - 1) msg'
                      else doStep n msg'
                | not owned -> do
                    iids <- getInvestigators
                    addCampaignCardToDeckChoice iids DoNotShuffleIn Assets.claypoolsFurs
                    doStep (n - 1) msg'
                | otherwise -> doStep n msg'

          when (ellsworth `elem` choices) do
            labeled' "roaldEllsworth" do
              choiceMade ellsworth
              let alive = ellsworth `elem` remainingPartners
              owned <- isJust <$> getOwner Assets.ellsworthsBoots
              blueStory
                $ compose
                  [ validateEntry alive "ellsworthIsAlive"
                  , hr
                  , validateEntry (not alive && not owned) "ellsworthIsCrossedOut"
                  ]
              if
                | alive -> do
                    record TheInvestigatorsScoutedTheForkedPass
                    doStep (n - 1) msg'
                | not owned -> do
                    iids <- getInvestigators
                    addCampaignCardToDeckChoice iids DoNotShuffleIn Assets.ellsworthsBoots
                    doStep (n - 1) msg'
                | otherwise -> doStep n msg'

          let takada = Assets.takadaHirokoAeroplaneMechanic.cardCode
          when (takada `elem` choices) do
            labeled' "takadaHiroko" do
              choiceMade takada
              let alive = takada `elem` remainingPartners
              owned <- isJust <$> getOwner Events.takadasCache
              blueStory
                $ compose
                  [ validateEntry alive "takadaIsAlive"
                  , hr
                  , validateEntry (not alive && not owned) "takadaIsCrossedOut"
                  ]
              if
                | alive -> do
                    iids <- getInvestigators
                    chooseOneM lead $ campaignI18n $ for_ iids \iid -> do
                      questionLabeled' "beginScenarioIIIWithExtraResources"
                      portraitLabeled iid do
                        scenarioSetupModifier "08621" CampaignSource iid (StartingResources 3)
                    doStep (n - 1) msg'
                | not owned -> do
                    iids <- getInvestigators
                    addCampaignCardToDeckChoice iids DoNotShuffleIn Events.takadasCache
                    doStep (n - 1) msg'
                | otherwise -> doStep n msg'

          let ashevak = Assets.eliyahAshevakDogHandler.cardCode
          when (ashevak `elem` choices) do
            labeled' "eliyahAshevak" do
              choiceMade ashevak
              let alive = ashevak `elem` remainingPartners
              owned <- isJust <$> getOwner Assets.anyuFaithfulCompanion
              blueStory
                $ compose
                  [ validateEntry alive "ashevakIsAlive"
                  , hr
                  , validateEntry (not alive && not owned) "ashevakIsCrossedOut"
                  ]
              if
                | alive -> do
                    injured <- select InvestigatorWithMentalTrauma
                    damagedPartners <- filter ((> 0) . (.horror)) <$> getRemainingPartners
                    if null injured && null damagedPartners
                      then doStep n msg'
                      else do
                        chooseOneM lead $ campaignI18n do
                          labeled' "doNotPerformHealing" nothing
                          for_ injured \iid -> portraitLabeled iid $ push $ HealTrauma iid 0 1
                          for_ damagedPartners \partner -> do
                            cardLabeled partner $ push $ HealHorror (CardCodeTarget partner.cardCode) CampaignSource 1
                        doStep (n - 1) msg'
                | not owned -> do
                    iids <- getInvestigators
                    addCampaignCardToDeckChoice iids DoNotShuffleIn Assets.anyuFaithfulCompanion
                    doStep (n - 1) msg'
                | otherwise -> doStep n msg'
      pure c
    CampaignStep (InterludeStepPart 3 _ 2) -> scope "interlude3" do
      let shouldFinalNight3 = FatalMirage `elem` campaignCompletedSteps attrs
      story
        $ withVars
          [ "finalNight3Status" .= String (if shouldFinalNight3 then "valid" else "invalid")
          , "finalNight4Status" .= String (if not shouldFinalNight3 then "valid" else "invalid")
          ]
        $ i18nWithTitle "finalNight2"
      push $ CampaignStep $ InterludeStepPart 3 Nothing $ if shouldFinalNight3 then 3 else 4
      pure c
    CampaignStep (InterludeStepPart 3 _ 3) -> scope "interlude3" do
      storyWithChooseOneM (i18nWithTitle "finalNight3") $ campaignI18n do
        labeled' "openTheDoorAndVentureOnceMore" do
          push $ NextCampaignStep $ continue FatalMirage
        labeled' "ignoreTheDoor" do
          push $ NextCampaignStep $ continue TheHeartOfMadnessPart1
      pure c
    CampaignStep (InterludeStepPart 3 _ 4) -> scope "interlude3" do
      storyWithChooseOneM (i18nWithTitle "finalNight4") $ campaignI18n do
        labeled' "openTheDoorAndVenture" do
          push $ NextCampaignStep $ continue FatalMirage
        labeled' "ignoreTheDoor" do
          push $ NextCampaignStep $ continue TheHeartOfMadnessPart1
      pure c
    CampaignStep (CheckpointStep 3) -> scope "checkpoint3" do
      story $ i18nWithTitle "theOtherSide1"
      story $ i18nWithTitle "theOtherSide3"
      push $ NextCampaignStep $ continue TheHeartOfMadnessPart2
      pure c
    CampaignStep EpilogueStep -> scope "epilogue" do
      story $ i18nWithTitle "epilogue"
      ellsworthIsAlive <- getPartnerIsAlive Assets.roaldEllsworthIntrepidExplorer
      claypoolIsAlive <- getPartnerIsAlive Assets.averyClaypoolAntarcticGuide
      when (ellsworthIsAlive && claypoolIsAlive) $ story $ i18n "ellsworthAndClaypool"

      kenslerIsAlive <- getPartnerIsAlive Assets.drAmyKenslerProfessorOfBiology
      sinhaIsAlive <- getPartnerIsAlive Assets.drMalaSinhaDaringPhysician
      when (kenslerIsAlive && sinhaIsAlive) $ story $ i18n "kenslerAndSinha"

      dyerIsAlive <- getPartnerIsAlive Assets.professorWilliamDyerProfessorOfGeology
      danforthIsAlive <- getPartnerIsAlive Assets.danforthBrilliantStudent
      when (dyerIsAlive && danforthIsAlive) $ story $ i18n "dyerAndDanforth"

      takadaIsAlive <- getPartnerIsAlive Assets.takadaHirokoAeroplaneMechanic
      cookieIsAlive <- getPartnerIsAlive Assets.jamesCookieFredericksDubiousChoice
      when (cookieIsAlive && takadaIsAlive) $ story $ i18n "takadaAndCookie"

      ashevakIsAlive <- getPartnerIsAlive Assets.eliyahAshevakDogHandler
      when
        ( not
            ( ellsworthIsAlive
                || claypoolIsAlive
                || kenslerIsAlive
                || sinhaIsAlive
                || dyerIsAlive
                || danforthIsAlive
                || cookieIsAlive
                || takadaIsAlive
            )
            && ashevakIsAlive
        )
        do
          story $ i18n "ashevak"

      push GameOver
      pure c
    SetPartnerStatus cCode status -> do
      pure $ EdgeOfTheEarth $ attrs & logL . partnersL . ix cCode . statusL .~ status
    When (AssetDefeated _ aid) -> do
      cCode <- field AssetCardCode aid
      for_ (toPartnerCodeMay cCode) \partnerCode ->
        push $ SetPartnerStatus partnerCode Eliminated
      pure c
    RemoveFromPlay (AssetSource aid) -> do
      mCode <- fieldMay AssetCardCode aid
      case mCode >>= toPartnerCodeMay of
        Nothing -> pure c
        Just partnerCode -> do
          damage <- field AssetDamage aid
          horror <- field AssetHorror aid
          pure
            $ EdgeOfTheEarth
            $ attrs
            & (logL . partnersL . ix partnerCode %~ ((damageL .~ damage) . (horrorL .~ horror)))
    RemoveFromGame (AssetTarget aid) -> do
      mCode <- fieldMay AssetCardCode aid
      case mCode >>= toPartnerCodeMay of
        Nothing -> pure c
        Just partnerCode -> do
          damage <- field AssetDamage aid
          horror <- field AssetHorror aid
          pure
            $ EdgeOfTheEarth
            $ attrs
            & (logL . partnersL . ix partnerCode %~ ((damageL .~ damage) . (horrorL .~ horror)))
    HealDamage (CardCodeTarget cCode) CampaignSource n -> do
      case toPartnerCodeMay cCode of
        Just partnerCode ->
          pure
            $ EdgeOfTheEarth
            $ attrs
            & (logL . partnersL . ix partnerCode %~ (damageL %~ max 0 . subtract n))
        Nothing -> pure c
    HealHorror (CardCodeTarget cCode) CampaignSource n -> do
      case toPartnerCodeMay cCode of
        Just partnerCode ->
          pure
            $ EdgeOfTheEarth
            $ attrs
            & (logL . partnersL . ix partnerCode %~ (horrorL %~ max 0 . subtract n))
        Nothing -> pure c
    CampaignStep TheHeartOfMadnessPart1 -> scope "theHeartOfMadness.part1" do
      story $ i18nWithTitle "intro"
      kenslerIsAlive <- getPartnerIsAlive Assets.drAmyKenslerProfessorOfBiology
      blueStory
        $ validateEntry kenslerIsAlive "kensler.alive"
        <> hr
        <> validateEntry (not kenslerIsAlive) "kensler.otherwise"

      unless kenslerIsAlive do
        eachInvestigator (`sufferPhysicalTrauma` 1)

      scoutedTheForkedPass <- getHasRecord TheInvestigatorsScoutedTheForkedPass
      blueStory
        $ validateEntry scoutedTheForkedPass "scoutedTheForkedPass.yes"
        <> hr
        <> validateEntry (not scoutedTheForkedPass) "scoutedTheForkedPass.no"

      danforthIsAlive <- getPartnerIsAlive Assets.danforthBrilliantStudent
      story
        $ i18n "hoursPass"
        <> blueFlavor
          ( validateEntry danforthIsAlive "danforth.alive"
              <> hr
              <> validateEntry (not danforthIsAlive) "danforth.otherwise"
          )

      unless danforthIsAlive do
        eachInvestigator (`sufferMentalTrauma` 1)

      miasmicCrystalRecovered <- hasSupply MiasmicCrystal
      blueStory
        $ validateEntry miasmicCrystalRecovered "miasmicCrystal.recovered"
        <> hr
        <> validateEntry (not miasmicCrystalRecovered) "miasmicCrystal.unrecovered"

      unless miasmicCrystalRecovered do
        when (count (== #frost) (campaignChaosBag attrs) < 8) $ addChaosToken #frost

      let extraCardsIids :: [InvestigatorId] =
            toResult
              $ Map.findWithDefault
                (toJSON ([] :: [InvestigatorId]))
                "heartOfMadnessExtraCards"
                (campaignStore attrs)

      storyWithChooseOneM (i18nWithTitle "proceed") $ campaignI18n do
        labeled' "stayAndStudyTheGreatDoor"
          do
            for_ extraCardsIids \iid ->
              scenarioSetupModifier "08648a" CampaignSource iid (StartingHand 2)
            pushAll
              [ ResetInvestigators
              , ResetGame
              , ForTarget GameTarget ResetGame
              , ForInvestigators [] ResetGame
              , StartScenario "08648a" Nothing
              ]
        labeled' "passThroughTheGate"
          do
            for_ extraCardsIids \iid ->
              scenarioSetupModifier "08648b" CampaignSource iid (StartingHand 2)
            push $ NextCampaignStep $ continue TheHeartOfMadnessPart2

      pure c
    EndOfGame _ -> do
      let updatePartner a' partner = do
            mAid <- selectOne (AssetIs partner.cardCode)
            case mAid of
              Nothing -> pure a'
              Just aid -> do
                damage <- field AssetDamage aid
                horror <- field AssetHorror aid
                pure
                  $ a'
                  & (logL . partnersL . ix (toPartnerCode partner) %~ ((damageL .~ damage) . (horrorL .~ horror)))
      updatedAttrs <- foldM updatePartner attrs =<< traverse getPartner expeditionTeam
      lift $ defaultCampaignRunner msg (EdgeOfTheEarth updatedAttrs)
    _ -> lift $ defaultCampaignRunner msg c
