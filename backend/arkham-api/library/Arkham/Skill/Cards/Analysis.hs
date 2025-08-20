module Arkham.Skill.Cards.Analysis (analysis) where

import Arkham.Ability
import Arkham.Helpers.SkillTest (withSkillTestInvestigator)
import Arkham.Helpers.Window (getChaosToken)
import Arkham.Matcher
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted hiding (RevealChaosToken)

newtype Analysis = Analysis SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

analysis :: SkillCard Analysis
analysis = skill Analysis Cards.analysis

instance HasAbilities Analysis where
  getAbilities (Analysis x) =
    [ displayAsAction
        $ noLimit
        $ controlled_ x 1
        $ ConstantReaction
          "Place 1 of your clues on your location (Analysis)"
          (RevealChaosToken #after Anyone $ CancelableChaosToken AnyChaosToken)
          (PlaceClueOnLocationCost 1)
    ]

instance RunMessage Analysis where
  runMessage msg s@(Analysis attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getChaosToken -> token) _ -> do
      withSkillTestInvestigator \iid' -> do
        cancelChaosToken (attrs.ability 1) iid token
        returnChaosTokens [token]
        unfocusChaosTokens
        drawAnotherChaosToken iid'
        push RerunSkillTest
      pure s
    _ -> Analysis <$> liftRunMessage msg attrs
