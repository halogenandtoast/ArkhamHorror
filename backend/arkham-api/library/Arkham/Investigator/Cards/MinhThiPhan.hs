module Arkham.Investigator.Cards.MinhThiPhan (minhThiPhan, MinhThiPhan (..)) where

import Arkham.Ability
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Skill.Types qualified as Field
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype MinhThiPhan = MinhThiPhan InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

minhThiPhan :: InvestigatorCard MinhThiPhan
minhThiPhan =
  investigator MinhThiPhan Cards.minhThiPhan
    $ Stats {health = 7, sanity = 7, willpower = 4, intellect = 4, combat = 2, agility = 2}

instance HasAbilities MinhThiPhan where
  getAbilities (MinhThiPhan attrs) =
    [ limitedAbility (PerInvestigatorLimit PerRound 1)
        $ restricted attrs 1 Self
        $ freeReaction
        $ CommittedCard #after (affectsOthers $ at_ YourLocation) AnyCard
    ]

instance HasChaosTokenValue MinhThiPhan where
  getChaosTokenValue iid ElderSign (MinhThiPhan attrs) | attrs `is` iid = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage MinhThiPhan where
  runMessage msg i@(MinhThiPhan attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 [(windowType -> Window.CommittedCard _ card)] _ -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) card (AddSkillIcons [#wild])
      pure i
    ElderSignEffect iid | iid == toId attrs -> do
      withSkillTest \sid -> do
        skills <- selectWithField Field.SkillCard AnySkill
        chooseOrRunOneM iid do
          labeled "Do not choose a skill to return to your hand" nothing
          for_ skills \(s, c) ->
            targeting c $ skillTestModifier sid (toSource ElderSign) s ReturnToHandAfterTest
      pure i
    _ -> MinhThiPhan <$> liftRunMessage msg attrs
