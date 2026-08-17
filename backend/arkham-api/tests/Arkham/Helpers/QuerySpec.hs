module Arkham.Helpers.QuerySpec (spec) where

import Arkham.Helpers.Query (getLead)
import Arkham.Investigator.Cards qualified as Investigators
import TestImport.New

{- | Regression for issue #5420. The Man in the Pallid Mask is added to "the
lead investigator's" deck at the end of Curtain Call, but the whole party had
resigned by then. 'LeadInvestigator' only matches uneliminated investigators,
so 'getLead' fell through to an arbitrary eliminated investigator — which is
whoever sorts first in the entity Map, i.e. the lowest investigator id. Per
FFG, the most recently appointed lead resolves the instruction, and the game
already records them in gameLeadInvestigatorId.
-}
spec :: Spec
spec = describe "getLead" do
  it "returns the lead investigator while they are still in play" . gameTest $ \self -> do
    void $ addInvestigator Investigators.rolandBanks
    getLead `shouldReturn` self.id

  it "returns the recorded lead once everyone is eliminated" . gameTest $ \self -> do
    -- Jenny (02003) is seeded as lead; Roland (01001) sorts before her in the
    -- entity Map, so the old fallback picked Roland.
    other <- addInvestigator Investigators.rolandBanks
    run $ Resign other.id
    run $ Resign self.id
    getLead `shouldReturn` self.id
