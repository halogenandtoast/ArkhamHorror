module Arkham.Skill.Cards.Purified (purified) where

import {-# SOURCE #-} Arkham.GameEnv (getSkillTest)
import Arkham.Helpers.ChaosBag (getRemainingBlessTokens)
import Arkham.Matcher
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.SkillTestResult

newtype Purified = Purified SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

purified :: SkillCard Purified
purified = skill Purified Cards.purified

instance RunMessage Purified where
  runMessage msg s@(Purified attrs) = runQueueT $ case msg of
    CheckSkillTestResultOptions skillTestId exclusions -> do
      mst <- getSkillTest
      for_ mst \st -> do
        when (st.id == skillTestId && isTarget attrs st.target) do
          case st.result of
            SucceededBy _ (min 5 -> n) | n > 0 -> do
              let passedMsg = PassedSkillTest st.investigator st.action st.source st.target st.kind n
              provideSkillTestResultOption attrs exclusions "Purified" $ doStep n passedMsg
            _ -> pure ()
      pure s
    DoStep n (PassedSkillTest iid _ _ (isTarget attrs -> True) _ _) -> do
      curse <- selectCount $ ChaosTokenFaceIs #curse
      bless <- getRemainingBlessTokens

      if
        | bless == 0 && curse == 0 -> pure ()
        | bless == 0 && curse /= 0 -> repeated (min curse n) $ removeChaosToken #curse
        | curse == 0 && bless /= 0 -> repeated (min bless n) $ addChaosToken #bless
        | bless + curse == n -> do
            repeated curse $ removeChaosToken #curse
            repeated bless $ addChaosToken #bless
        | otherwise -> do
            chooseAmounts
              iid
              "Add Bless Tokens or Remove Curse Tokens"
              (TotalAmountTarget $ min n (bless + curse))
              [("Add Bless Tokens", (0, bless)), ("Remove Curse Tokens", (0, curse))]
              attrs
      pure s
    ResolveAmounts _iid choices (isTarget attrs -> True) -> do
      let
        bless = getChoiceAmount "Add Bless Tokens" choices
        curse = getChoiceAmount "Remove Curse Tokens" choices

      repeated curse $ removeChaosToken #curse
      repeated bless $ addChaosToken #bless
      pure s
    _ -> Purified <$> liftRunMessage msg attrs
