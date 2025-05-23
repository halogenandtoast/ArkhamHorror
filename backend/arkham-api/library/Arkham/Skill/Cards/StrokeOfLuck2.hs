module Arkham.Skill.Cards.StrokeOfLuck2 (strokeOfLuck2) where

import Arkham.ChaosToken
import Arkham.Message.Lifted.Choose
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype StrokeOfLuck2 = StrokeOfLuck2 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strokeOfLuck2 :: SkillCard StrokeOfLuck2
strokeOfLuck2 = skill StrokeOfLuck2 Cards.strokeOfLuck2

instance RunMessage StrokeOfLuck2 where
  runMessage msg s@(StrokeOfLuck2 attrs) = runQueueT $ case msg of
    RevealChaosToken _ iid token | chaosTokenFace token /= AutoFail -> do
      chooseOneM iid do
        labeled "Exile Stroke of Luck to automatically succeed" do
          exile attrs
          passSkillTest
        labeled "Do not exile" nothing
      pure s
    _ -> StrokeOfLuck2 <$> liftRunMessage msg attrs
