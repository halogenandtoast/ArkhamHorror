module Arkham.Debug.CampaignLogSpec (spec) where

import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.Helpers.Log (getCampaignLog)
import Arkham.Projection (field)
import TestImport.New

note :: CampaignLogKey
note = HomebrewCampaignLogKey "custom.The Lodge owes us"

spec :: Spec
spec = describe "Debug campaign log, trauma and XP" do
  it "forgets a recorded custom note" . gameTest $ \_ -> do
    run $ Record note
    (member note . campaignLogRecorded <$> getCampaignLog) `shouldReturn` True
    run $ ForgetRecord note
    (member note . campaignLogRecorded <$> getCampaignLog) `shouldReturn` False

  it "forgets a custom count" . gameTest $ \_ -> do
    run $ RecordCount note 3
    (lookup note . campaignLogRecordedCounts <$> getCampaignLog) `shouldReturn` Just 3
    run $ ForgetRecord note
    (lookup note . campaignLogRecordedCounts <$> getCampaignLog) `shouldReturn` Nothing

  it "suffers and heals trauma" . gameTest $ \self -> do
    run $ SufferTrauma (toId self) 1 0
    field InvestigatorPhysicalTrauma (toId self) `shouldReturn` 1
    run $ HealTrauma (toId self) 1 0
    field InvestigatorPhysicalTrauma (toId self) `shouldReturn` 0

  it "spending more XP than there is stops at zero" . gameTest $ \self -> do
    run $ GainXP (toId self) GameSource 2
    field InvestigatorXp (toId self) `shouldReturn` 2
    run $ SpendXP (toId self) 5
    field InvestigatorXp (toId self) `shouldReturn` 0
