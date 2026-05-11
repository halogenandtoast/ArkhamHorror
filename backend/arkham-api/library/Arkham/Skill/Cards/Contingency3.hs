module Arkham.Skill.Cards.Contingency3 (contingency3) where

import Arkham.Ability
import Arkham.Helpers.SkillTest (withSkillTestInvestigator)
import Arkham.Helpers.Window (getRevealedChaosTokens)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted hiding (RevealChaosToken)

newtype Contingency3 = Contingency3 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

contingency3 :: SkillCard Contingency3
contingency3 = skill Contingency3 Cards.contingency3

instance HasAbilities Contingency3 where
  getAbilities (Contingency3 x) =
    [ displayAsAction
        $ noLimit
        $ controlled_ x 1
        $ ConstantReaction
          "Cancel a revealed chaos token (Contingency)"
          (RevealChaosTokensDuringSkillTest #cancel Anyone (WhileTargetingEnemy AnyEnemy) #any)
          (ResourceCost 3)
    ]

instance RunMessage Contingency3 where
  runMessage msg s@(Contingency3 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getRevealedChaosTokens -> tokens) _ -> do
      case tokens of
        [token] -> handleTarget iid (attrs.ability 1) token
        _ -> chooseTargetM iid tokens $ handleTarget iid (attrs.ability 1)
      pure s
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (ChaosTokenTarget token) -> do
      cancelChaosToken (attrs.ability 1) iid token
      returnChaosTokens [token]
      unfocusChaosTokens
      withSkillTestInvestigator drawAnotherChaosToken
      push RerunSkillTest
      pure s
    _ -> Contingency3 <$> liftRunMessage msg attrs
