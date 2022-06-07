module Arkham.Skill.Cards.StrokeOfLuck2
  ( strokeOfLuck2
  , StrokeOfLuck2(..)
  ) where

import Arkham.Prelude

import Arkham.Skill.Cards qualified as Cards
import Arkham.Classes
import Arkham.Message
import Arkham.Skill.Runner
import Arkham.Token

newtype StrokeOfLuck2 = StrokeOfLuck2 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strokeOfLuck2 :: SkillCard StrokeOfLuck2
strokeOfLuck2 = skill StrokeOfLuck2 Cards.strokeOfLuck2

instance RunMessage StrokeOfLuck2 where
  runMessage msg s@(StrokeOfLuck2 attrs) = case msg of
    RevealToken _ iid token | tokenFace token /= AutoFail -> s <$ push
      (chooseOne
        iid
        [ Label
          "Exile Stroke of Luck to automatically succeed"
          [Exile (toTarget attrs), PassSkillTest]
        , Label "Do not exile" []
        ]
      )
    _ -> StrokeOfLuck2 <$> runMessage msg attrs
