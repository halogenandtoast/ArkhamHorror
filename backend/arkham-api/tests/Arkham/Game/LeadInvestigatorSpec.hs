module Arkham.Game.LeadInvestigatorSpec (spec) where

import Arkham.Helpers.Query (getRecordedLead)
import Arkham.Investigator.Cards qualified as Investigators
import TestImport.Lifted

-- #5779: agenda 3b "They're Getting Out" defeats every investigator at once. The
-- lead's defeat resolved its ChooseLeadInvestigator while the others were still
-- standing, so one of them was promoted moments before being defeated herself —
-- and the resolution offered Lita Chantler to her instead of to the lead.
spec :: Spec
spec = describe "lead investigator" $ do
  it "keeps the recorded lead when everyone is defeated at once" $ gameTest $ \self -> do
    other <- addInvestigator Investigators.rolandBanks
    parkNoRemainingInvestigators
    defeatAll [self, other]
    getRecordedLead `shouldReturn` Just (toId self)

  it "promotes the survivor, not an investigator defeated alongside the lead" $ gameTest $ \self -> do
    doomed <- addInvestigator Investigators.rolandBanks
    survivor <- addInvestigator Investigators.daisyWalker
    defeatAll [self, doomed]
    getRecordedLead `shouldReturn` Just (toId survivor)

  it "still promotes the survivor when only the lead is defeated" $ gameTest $ \self -> do
    other <- addInvestigator Investigators.rolandBanks
    defeatAll [self]
    getRecordedLead `shouldReturn` Just (toId other)
 where
  -- One batch, the way `eachInvestigator` queues an "each investigator is
  -- defeated" agenda.
  defeatAll :: [Investigator] -> TestAppT ()
  defeatAll = runAll . map (InvestigatorDefeated (TestSource mempty) . toId)

  -- Losing every investigator otherwise clears the queue and drops the test into
  -- The Gathering's no-resolution, which has nothing to do with the bookkeeping
  -- under test. Point the handler somewhere the scenario won't claim.
  parkNoRemainingInvestigators :: TestAppT ()
  parkNoRemainingInvestigators = run $ SetNoRemainingInvestigatorsHandler TestTarget
