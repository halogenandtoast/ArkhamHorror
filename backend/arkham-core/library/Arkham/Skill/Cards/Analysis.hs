module Arkham.Skill.Cards.Analysis (analysis, Analysis (..)) where

import Arkham.Ability
import Arkham.Game.Helpers (cancelChaosToken)
import Arkham.Helpers.SkillTest (getSkillTestInvestigator)
import Arkham.Helpers.Window (getChaosToken)
import Arkham.Matcher
import Arkham.Message (MessageType (..))
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
        $ limitedAbility NoLimit
        $ restrictedAbility x 1 ControlsThis
        $ ConstantReaction
          "Place 1 of your clues on your location (Analysis)"
          (RevealChaosToken #after Anyone AnyChaosToken)
          (PlaceClueOnLocationCost 1)
    ]

instance RunMessage Analysis where
  runMessage msg s@(Analysis attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (getChaosToken -> token) _ -> do
      whenJustM getSkillTestInvestigator \iid' -> do
        cancelChaosToken token
        pushAll
          [ CancelEachNext (toSource attrs) [RunWindowMessage, DrawChaosTokenMessage, RevealChaosTokenMessage]
          , ReturnChaosTokens [token]
          , UnfocusChaosTokens
          , DrawAnotherChaosToken iid'
          , RerunSkillTest
          ]
      pure s
    _ -> Analysis <$> liftRunMessage msg attrs
