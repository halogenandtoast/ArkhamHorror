module Arkham.Investigator.Cards.FatherMateoParallelSpec (spec) where

import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Investigator.Runner (setMetaKey)
import Arkham.Projection (field)
import TestImport.New

spec :: Spec
spec = describe "Father Mateo (Parallel)" $ do
  context "When you would reveal a chaos token" $ do
    it "lets you reveal a bless sealed here instead even if the pending guard leaked (#4774)"
      . gameTestWith Investigators.fatherMateoParallel
      $ \self -> do
        blessId <- getRandom
        let blessToken = ChaosToken blessId BlessToken Nothing False False
        -- Reproduce the bugged save state: a bless sealed on Mateo that is also
        -- stuck in the "father_mateo_pending" guard. With the leaked entry the
        -- only sealed bless was excluded from the reaction's matcher, so the
        -- reaction could never be triggered again.
        _ <- updateThis self $ \attrs ->
          setMetaKey "father_mateo_pending" [blessToken]
            $ attrs {investigatorSealedChaosTokens = [blessToken]}
        setChaosTokens [Skull]

        sid <- getRandom
        run $ beginSkillTest sid self SkillIntellect 2
        startSkillTest
        -- Before the fix this reaction was not offered at all.
        useReaction
        -- The sealed bless is revealed in place of the drawn token, so it is no
        -- longer sealed on Mateo.
        field InvestigatorSealedChaosTokens self.id `shouldReturn` []
