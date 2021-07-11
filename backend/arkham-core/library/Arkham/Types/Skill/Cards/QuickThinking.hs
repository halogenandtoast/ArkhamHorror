module Arkham.Types.Skill.Cards.QuickThinking
  ( quickThinking
  , QuickThinking(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Skill.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Runner
import Arkham.Types.Target

newtype QuickThinking = QuickThinking SkillAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quickThinking :: SkillCard QuickThinking
quickThinking = skill QuickThinking Cards.quickThinking

instance HasModifiersFor env QuickThinking

instance HasActions env QuickThinking where
  getActions iid window (QuickThinking attrs) = getActions iid window attrs

instance SkillRunner env => RunMessage env QuickThinking where
  runMessage msg s@(QuickThinking attrs) = case msg of
    PassedSkillTest iid _ _ SkillTestInitiatorTarget{} _ n | n >= 2 -> s <$ push
      (chooseOne
        iid
        [ Label "Take additional action" [PlayerWindow iid [] True]
        , Label "Pass on additional action" []
        ]
      )
    _ -> QuickThinking <$> runMessage msg attrs
