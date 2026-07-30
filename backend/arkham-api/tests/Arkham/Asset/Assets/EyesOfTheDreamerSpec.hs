module Arkham.Asset.Assets.EyesOfTheDreamerSpec (spec) where

import Arkham.Ability.Type (AbilityType (..))
import Arkham.Ability.Types (Ability (..))
import Arkham.Asset.Cards qualified as Assets
import Arkham.ChaosBagStepState
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Projection (field)
import TestImport.New

spec :: Spec
spec = describe "Eyes of the Dreamer" $ do
  context "when parallel Father Mateo replaces one of the revealed tokens" $ do
    it "still lets you choose the sealed bless to resolve (#5306)"
      . gameTestWith Investigators.fatherMateoParallel
      $ \self -> do
        blessId <- getRandom
        let blessToken = ChaosToken blessId BlessToken Nothing False False
        _ <- updateThis self $ \attrs -> attrs {investigatorSealedChaosTokens = [blessToken]}
        withProp @"willpower" 5 self
        location <- testLocation & prop @"clues" 1 & prop @"shroud" 0
        self `moveTo` location
        eyes <- self `putAssetIntoPlay` Assets.eyesOfTheDreamer
        setChaosTokens [Zero]

        [doInvestigate] <- self `getActionsFrom` eyes
        self `useAbility` doInvestigate
        startSkillTest

        -- Spend 1 charge to reveal 1 additional chaos token
        chooseOptionMatching "spend charges on Eyes of the Dreamer" \case
          AbilityLabel {ability} -> case abilityType ability of
            ConstantReaction {} -> True
            _ -> False
          _ -> False
        payUpTo 3 1

        -- Mateo resolves the bless sealed on him in place of one of the draws
        useReactionOf self

        -- The bless is one of the revealed tokens, so it must be offered as
        -- something you can choose to resolve. Before the fix it was silently
        -- dropped: a sealed token is in neither the bag nor the set-aside pile,
        -- so it matched no matcher and its group was filtered out.
        chooseOptionMatching "resolve the bless token" \case
          ChaosTokenGroupChoice _ _ (ChooseMatch _ 1 _ _ [[ChaosToken _ BlessToken _ _ _]] _ _) -> True
          _ -> False

        -- Resolving it releases it
        field InvestigatorSealedChaosTokens self.id `shouldReturn` []
