module Arkham.Investigator.Cards.MinhThiPhan (
  minhThiPhan,
  MinhThiPhan (..),
) where

import Arkham.Ability
import Arkham.Card
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Skill.Types qualified as Field
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype MinhThiPhan = MinhThiPhan InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

minhThiPhan :: InvestigatorCard MinhThiPhan
minhThiPhan =
  investigator MinhThiPhan Cards.minhThiPhan
    $ Stats {health = 7, sanity = 7, willpower = 4, intellect = 4, combat = 2, agility = 2}

instance HasAbilities MinhThiPhan where
  getAbilities (MinhThiPhan attrs) =
    [ playerLimit PerRound
        $ restrictedAbility attrs 1 Self
        $ freeReaction
        $ CommittedCard #after (affectsOthers $ InvestigatorAt YourLocation) AnyCard
    ]

instance HasChaosTokenValue MinhThiPhan where
  getChaosTokenValue iid ElderSign (MinhThiPhan attrs) | attrs `is` iid = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage MinhThiPhan where
  runMessage msg i@(MinhThiPhan attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 [(windowType -> Window.CommittedCard _ card)] _ -> do
      push $ skillTestModifier (toAbilitySource attrs 1) (toCardId card) (AddSkillIcons [#wild])
      pure i
    ResolveChaosToken _ ElderSign iid | iid == toId attrs -> do
      skills <- selectWithField Field.SkillCard AnySkill
      player <- getPlayer iid
      pushWhen (notNull skills)
        $ chooseOne player
        $ Label "Do not choose a skill to return to your hand" []
        : [targetLabel c [skillTestModifier (toSource ElderSign) s ReturnToHandAfterTest] | (s, c) <- skills]
      pure i
    _ -> MinhThiPhan <$> runMessage msg attrs
