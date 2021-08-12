module Arkham.Types.Skill.Cards.StrokeOfLuck2
  ( strokeOfLuck2
  , StrokeOfLuck2(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Skill.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Runner
import Arkham.Types.Token

newtype StrokeOfLuck2 = StrokeOfLuck2 SkillAttrs
  deriving anyclass IsSkill
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strokeOfLuck2 :: SkillCard StrokeOfLuck2
strokeOfLuck2 = skill StrokeOfLuck2 Cards.strokeOfLuck2

instance HasModifiersFor env StrokeOfLuck2
instance HasActions StrokeOfLuck2

instance SkillRunner env => RunMessage env StrokeOfLuck2 where
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
