module Arkham.Campaign.Campaigns.EdgeOfTheEarth (EdgeOfTheEarth (..), edgeOfTheEarth) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Campaign.Import.Lifted
import Arkham.Campaign.Types (campaignChaosBag, campaignCompletedSteps, campaignStore)
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.Campaigns.EdgeOfTheEarth.CampaignSteps
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Card
import Arkham.ChaosToken
import Arkham.EncounterSet (EncounterSet (Tekelili))
import Arkham.Event.Cards qualified as Events
import Arkham.FlavorText
import Arkham.Helpers.Log
import Arkham.Helpers.Query (getInvestigators, getLead)
import Arkham.Helpers.Text
import Arkham.Helpers.Xp (toBonus)
import Arkham.Matcher hiding (AssetDefeated)
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Projection
import Arkham.Scenarios.IceAndDeath.Helpers
import Arkham.Source
import Arkham.Target
import Data.Map.Strict qualified as Map

newtype EdgeOfTheEarth = EdgeOfTheEarth CampaignAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor)

edgeOfTheEarth :: Difficulty -> EdgeOfTheEarth
edgeOfTheEarth difficulty =
  campaign EdgeOfTheEarth (CampaignId "08") "Edge of the Earth" difficulty chaosBagContents
 where
{- FOURMOLU_DISABLE -}
  chaosBagContents = case difficulty of
    Easy -> [#"+1", #"+1", #"+1", #"0", #"0", #"-1", #"-1", #"-1", #"-2", #"-2", Skull, Skull, Cultist, Tablet, AutoFail, ElderSign]
    Standard -> [#"+1", #"0", #"0", #"-1", #"-1", #"-1", #"-2", #"-2", #"-3", #"-4", #frost, Skull, Skull, Cultist, Tablet, AutoFail, ElderSign]
    Hard -> [#"0", #"0", #"-1", #"-1", #"-2", #"-2", #"-3", #"-4", #"-4", #"-5", #frost, #frost, Skull, Skull, Cultist, Tablet, AutoFail, ElderSign]
    Expert -> [#"0", #"-1", #"-2", #"-2", #"-3", #"-4", #"-4", #"-5", #"-7", #frost, #frost, #frost, Skull, Skull, Cultist, Tablet, AutoFail, ElderSign]
{- FOURMOLU_ENABLE -}

instance IsCampaign EdgeOfTheEarth where
  nextStep a = case campaignStep (toAttrs a) of
    PrologueStep -> Just IceAndDeathPart1
    IceAndDeathPart1 -> Just (UpgradeDeckStep $ CheckpointStep 1)
    IceAndDeathPart2 -> Just (UpgradeDeckStep $ CheckpointStep 2)
    IceAndDeathPart3 -> Just (UpgradeDeckStep $ InterludeStep 1 Nothing)
    FatalMirage ->
      if
        | CityOfTheElderThings `elem` campaignCompletedSteps (toAttrs a) ->
            Just (UpgradeDeckStep TheHeartOfMadness)
        | ToTheForbiddenPeaks `elem` campaignCompletedSteps (toAttrs a) ->
            Just (UpgradeDeckStep CityOfTheElderThings)
        | otherwise -> Just (UpgradeDeckStep ToTheForbiddenPeaks)
    EpilogueStep -> Nothing
    UpgradeDeckStep nextStep' -> Just nextStep'
    _ -> Nothing

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
        labeled
          "“I believe you…but if what you say is true, should we not investigate these findings further?” Proceed to _Prologue 2._"
          $ doStep 2
          $ CampaignStep PrologueStep
        labeled "“I’m sorry, but this seems too wild to be true.” Skip to _Prologue 3._"
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
        labeled "Read _Partner_ intros" do
          storyWithCard Assets.drAmyKenslerProfessorOfBiology $ i18n "amyKensler"
          storyWithCard Assets.roaldEllsworthIntrepidExplorer $ i18n "roaldEllsworth"
          storyWithCard Assets.jamesCookieFredericksDubiousChoice $ i18n "jamesFredericks"
          storyWithCard Assets.takadaHirokoAeroplaneMechanic $ i18n "takadaHiroko"
          storyWithCard Assets.averyClaypoolAntarcticGuide $ i18n "averyClaypool"
          storyWithCard Assets.drMalaSinhaDaringPhysician $ i18n "malaSinha"
          storyWithCard Assets.eliyahAshevakDogHandler $ i18n "eliyahAshevak"
          storyWithCards [Assets.professorWilliamDyerProfessorOfGeology, Assets.danforthBrilliantStudent]
            $ i18n "williamDyer"
        labeled "Skip _Partner_ intros" nothing
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
          storyWithChooseOneM (i18nWithTitle "theDisappearance1") do
            labeled "They’re on their own." do
              for_ mia \partner -> do
                push $ SetPartnerStatus partner.cardCode Eliminated
              doStep 2 msg
            labeled "Go after the missing team members." do
              for_ mia \partner -> do
                push $ SetPartnerStatus partner.cardCode Mia
              doStep 3 msg
      pure c
    DoStep 2 (CampaignStep (CheckpointStep 1)) -> scope "checkpoint1" do
      story $ i18nWithTitle "theDisappearance2"
      push $ NextCampaignStep (Just $ CheckpointStep 2)
      pure c
    DoStep 3 (CampaignStep (CheckpointStep 1)) -> scope "checkpoint1" do
      story $ i18nWithTitle "theDisappearance3"
      push $ NextCampaignStep (Just IceAndDeathPart2)
      pure c
    CampaignStep (CheckpointStep 2) -> scope "checkpoint2" do
      storyWithChooseOneM (i18nWithTitle "theAttack1") do
        labeled "Run for your lives!" $ doStep 2 msg
        labeled "Stand and fight!" $ doStep 3 msg
      pure c
    DoStep 2 (CampaignStep (CheckpointStep 2)) -> scope "checkpoint2" do
      story $ i18nWithTitle "theAttack2"
      record TheTeamFledToTheMountains
      push $ NextCampaignStep $ Just $ InterludeStep 1 Nothing
      pure c
    DoStep 3 (CampaignStep (CheckpointStep 2)) -> scope "checkpoint2" do
      story $ i18nWithTitle "theAttack3"
      push $ NextCampaignStep $ Just IceAndDeathPart3
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
        else push $ NextCampaignStep $ Just ToTheForbiddenPeaks
      pure c
    CampaignStep (InterludeStepPart 1 _ 3) -> scope "interlude1" do
      storyWithChooseOneM (i18nWithTitle "restfulNight3") do
        labeled "Open the door and venture into the mirage."
          $ push
          $ NextCampaignStep
          $ Just FatalMirage
        labeled "Ignore the door and allow it to vanish"
          $ push
          $ NextCampaignStep
          $ Just ToTheForbiddenPeaks
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
        remainingPartners <- map (.cardCode) <$> getRemainingPartners
        let choiceMade choice = push $ SetGlobal CampaignTarget "interlude1" (toJSON $ filter (/= choice) choices)
        chooseOneM lead do
          questionLabeled $ "You can still check " <> tshow n <> " team members"
          let dyer = Assets.professorWilliamDyerProfessorOfGeology.cardCode
          when (dyer `elem` choices) do
            labeled "William Dyer" do
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
                      chooseOneM lead $ for_ iids \iid -> do
                        questionLabeled
                          "Any one investigator may choose and remove up to five Tekeli-li! weaknesses from their deck (*shuffling them with the remainder of the Tekeli-li encounter set*)."
                        portraitLabeled iid do
                          cards <- select $ inDeckOf iid <> basic (CardFromEncounterSet Tekelili)
                          focusCards cards \unfocus -> do
                            chooseUpToNM iid 5 "Do not remove anymore" do
                              targets cards $ removeCardFromDeckForCampaign iid
                            push unfocus

                      doStep (n - 1) msg'
                else do
                  iids <- getInvestigators
                  addCampaignCardToDeckChoice iids Events.dyersSketches
                  doStep (n - 1) msg'

          let danforth = Assets.danforthBrilliantStudent.cardCode
          when (danforth `elem` choices) do
            labeled "Danforth" do
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
                  chooseOneM lead $ for_ iids \iid -> do
                    questionLabeled
                      "Any one investigator may begin _Scenario II: To the Forbidden Peaks_ with two additional cards drawn in their opening hand."
                    portraitLabeled iid do
                      scenarioSetupModifier "08596" CampaignSource iid (StartingHand 2)
                else do
                  iids <- getInvestigators
                  addCampaignCardToDeckChoice iids Assets.collectedWorksOfPoe
              doStep (n - 1) msg'

          let kensler = Assets.drAmyKenslerProfessorOfBiology.cardCode
          when (kensler `elem` choices) do
            labeled "Dr. Amy Kensler" do
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
                  addCampaignCardToDeckChoice iids Assets.kenslersLog
              doStep (n - 1) msg'

          let sinha = Assets.drMalaSinhaDaringPhysician.cardCode
          when (sinha `elem` choices) do
            labeled "Dr. Mala Sinha" do
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
                      chooseOneM lead do
                        labeled "Do not perform healing" nothing
                        for_ injured \iid ->
                          portraitLabeled iid $ push $ HealTrauma iid 1 0
                        for_ damagedPartners \partner -> do
                          cardLabeled partner $ push $ HealDamage (CardCodeTarget $ partner.cardCode) CampaignSource 1
                      doStep (n - 1) msg'
                else do
                  iids <- getInvestigators
                  addCampaignCardToDeckChoice iids Assets.sinhasMedicalKit
                  doStep (n - 1) msg'

          let cookie = Assets.jamesCookieFredericksDubiousChoice.cardCode
          when (cookie `elem` choices) do
            labeled "James \"Cookie\" Fredericks" do
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
                  chooseOneM lead $ for_ iids \iid -> do
                    questionLabeled "Any one investigator earns 1 bonus experience."
                    portraitLabeled iid do
                      interludeXp iid $ toBonus "cookiesAdvice" 1
                else do
                  iids <- getInvestigators
                  addCampaignCardToDeckChoice iids Assets.cookiesCustom32
              doStep (n - 1) msg'

          let ellsworth = Assets.roaldEllsworthIntrepidExplorer.cardCode
          let ellsworthAlive = ellsworth `elem` remainingPartners

          let claypool = Assets.averyClaypoolAntarcticGuide.cardCode
          when (claypool `elem` choices) do
            labeled "Avery Claypool" do
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
                  addCampaignCardToDeckChoice iids Assets.claypoolsFurs
                  doStep (n - 1) msg'

          when (ellsworth `elem` choices) do
            labeled "Roald Ellsworth" do
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
                  addCampaignCardToDeckChoice iids Assets.ellsworthsBoots
              doStep (n - 1) msg'

          let takada = Assets.takadaHirokoAeroplaneMechanic.cardCode
          when (takada `elem` choices) do
            labeled "Takada Hiroko" do
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
                  chooseOneM lead $ for_ iids \iid -> do
                    questionLabeled
                      "Any one investigator may begin _Scenario II: To the Forbidden Peaks_ with 3 additional resources in their resource pool."
                    portraitLabeled iid do
                      scenarioSetupModifier "08596" CampaignSource iid (StartingResources 3)
                else do
                  iids <- getInvestigators
                  addCampaignCardToDeckChoice iids Events.takadasCache
              doStep (n - 1) msg'

          let ashevak = Assets.eliyahAshevakDogHandler.cardCode
          when (ashevak `elem` choices) do
            labeled "Eliyah Ashevak" do
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
                      chooseOneM lead do
                        labeled "Do not perform healing" nothing
                        for_ injured \iid ->
                          portraitLabeled iid $ push $ HealTrauma iid 0 1
                        for_ damagedPartners \partner -> do
                          cardLabeled partner $ push $ HealHorror (CardCodeTarget $ partner.cardCode) CampaignSource 1
                      doStep (n - 1) msg'
                else do
                  iids <- getInvestigators
                  addCampaignCardToDeckChoice iids Assets.anyuFaithfulCompanion
                  doStep (n - 1) msg'

      pure c
    CampaignStep (InterludeStep 2 _) -> scope "interlude1" do
      story $ i18nWithTitle "restfulNight2"
      pure c
    SetPartnerStatus cCode status -> do
      pure $ EdgeOfTheEarth $ attrs & logL . partnersL . ix cCode . statusL .~ status
    When (AssetDefeated _ aid) -> do
      cCode <- field AssetCardCode aid
      pushWhen (cCode `elem` map (.cardCode) expeditionTeam) $ SetPartnerStatus cCode Eliminated
      pure c
    RemoveFromGame (AssetTarget aid) -> do
      cCode <- field AssetCardCode aid
      if cCode `elem` map (.cardCode) expeditionTeam
        then do
          damage <- field AssetDamage aid
          horror <- field AssetHorror aid
          pure
            $ EdgeOfTheEarth
            $ attrs
            & logL
            . partnersL
            . ix cCode
            %~ (\partner -> partner & damageL .~ damage & horrorL .~ horror)
        else pure c
    HealDamage (CardCodeTarget cCode) CampaignSource n -> do
      if cCode `elem` map (.cardCode) expeditionTeam
        then do
          pure
            $ EdgeOfTheEarth
            $ attrs
            & logL
            . partnersL
            . ix cCode
            %~ (\partner -> partner & damageL %~ max 0 . subtract n)
        else pure c
    HealHorror (CardCodeTarget cCode) CampaignSource n -> do
      if cCode `elem` map (.cardCode) expeditionTeam
        then do
          pure
            $ EdgeOfTheEarth
            $ attrs
            & logL
            . partnersL
            . ix cCode
            %~ (\partner -> partner & horrorL %~ max 0 . subtract n)
        else pure c
    _ -> lift $ defaultCampaignRunner msg c
