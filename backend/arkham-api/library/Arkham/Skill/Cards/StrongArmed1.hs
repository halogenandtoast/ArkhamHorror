module Arkham.Skill.Cards.StrongArmed1 (strongArmed1) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified_)
import Arkham.Helpers.SkillTest (getSkillTestSource, withSkillTestInvestigator)
import Arkham.Helpers.Window (getChaosToken)
import Arkham.Matcher
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted hiding (RevealChaosToken)
import Arkham.Strategy

newtype StrongArmed1 = StrongArmed1 SkillAttrs
  deriving anyclass IsSkill
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strongArmed1 :: SkillCard StrongArmed1
strongArmed1 = skill StrongArmed1 Cards.strongArmed1

instance HasAbilities StrongArmed1 where
  getAbilities (StrongArmed1 x) =
    [ displayAsAction
        $ noLimit
        $ controlled_ x 1
        $ ConstantReaction
          "Take 1 damage (Strong-Armed)"
          (RevealChaosToken #after Anyone AnyChaosToken)
          (InvestigatorDamageCost (toSource x) (InvestigatorWithId x.controller) DamageAny 1)
    ]

instance HasModifiersFor StrongArmed1 where
  getModifiersFor (StrongArmed1 a) = do
    withSkillTestInvestigator \iid -> do
      maybeModified_ a iid do
        source <- MaybeT getSkillTestSource
        asset <- hoistMaybe source.asset
        liftGuardM $ asset <=~> asset_ (oneOf [#melee, #ranged])
        pure [DamageDealt 1]

instance RunMessage StrongArmed1 where
  runMessage msg s@(StrongArmed1 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getChaosToken -> token) _ -> do
      withSkillTestInvestigator \iid' -> do
        cancelChaosToken (attrs.ability 1) iid token
        returnChaosTokens [token]
        unfocusChaosTokens
        drawAnotherChaosToken iid'
        push RerunSkillTest
      pure s
    _ -> StrongArmed1 <$> liftRunMessage msg attrs
