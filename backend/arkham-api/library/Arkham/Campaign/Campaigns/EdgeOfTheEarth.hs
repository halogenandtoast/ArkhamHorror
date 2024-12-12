module Arkham.Campaign.Campaigns.EdgeOfTheEarth (EdgeOfTheEarth (..), edgeOfTheEarth) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Campaign.Import.Lifted
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.Campaigns.EdgeOfTheEarth.CampaignSteps
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.ChaosToken
import Arkham.Helpers.Query (getLead)
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.IceAndDeath.Helpers
import Arkham.Target

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
    IceAndDeathPart1 -> Just (CheckpointStep 1)
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
      story $ i18nWithTitle "prologue4"
      lead <- getLead
      chooseOneM lead do
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
      story $ i18nWithTitle "theDisappearance1"
      sv <- fromJustNote "missing shelter" <$> getCurrentShelterValue
      mia <- drop sv <$> (shuffle =<< getRemainingPartners)
      if null mia
        then doStep 2 msg
        else do
          lead <- getLead
          chooseOneM lead do
            labeled "They’re on their own." do
              for_ mia \partner -> do
                push $ SetPartnerStatus partner.cardCode Eliminated
              doStep 2 msg
            labeled "Go after the missing team members." do
              for_ mia \partner -> do
                push $ SetPartnerStatus partner.cardCode Mia
              doStep 3 msg
      pure c
    DoStep 2 (CampaignStep (CheckpointStep 1)) -> do
      story $ i18nWithTitle "theDisappearance2"
      push $ CampaignStep (CheckpointStep 2)
      pure c
    DoStep 3 (CampaignStep (CheckpointStep 1)) -> do
      story $ i18nWithTitle "theDisappearance3"
      push $ CampaignStep IceAndDeathPart2
      pure c
    CampaignStep (CheckpointStep 2) -> scope "checkpoint2" do
      story $ i18nWithTitle "theAttack1"
      lead <- getLead
      chooseOneM lead do
        labeled "Run for your lives!" $ doStep 2 msg
        labeled "Stand and fight!" $ doStep 3 msg
      pure c
    DoStep 2 (CampaignStep (CheckpointStep 2)) -> scope "checkpoint2" do
      story $ i18nWithTitle "theAttack2"
      record TheTeamFledToTheMountains
      push $ CampaignStep $ InterludeStep 1 Nothing
      pure c
    DoStep 3 (CampaignStep (CheckpointStep 2)) -> scope "checkpoint2" do
      story $ i18nWithTitle "theAttack3"
      push $ CampaignStep IceAndDeathPart3
      pure c
    CampaignStep (InterludeStep 1 _) -> scope "interlude1" do
      story $ i18nWithTitle "restfulNight1"
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
    _ -> lift $ defaultCampaignRunner msg c
