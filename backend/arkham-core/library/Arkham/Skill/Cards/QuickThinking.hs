module Arkham.Skill.Cards.QuickThinking
  ( quickThinking
  , QuickThinking(..)
  ) where

import Arkham.Prelude

import Arkham.Skill.Cards qualified as Cards
import Arkham.Card.CardCode
import Arkham.Classes
import Arkham.Message
import Arkham.Skill.Runner
import Arkham.Target

newtype QuickThinking = QuickThinking SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quickThinking :: SkillCard QuickThinking
quickThinking = skill QuickThinking Cards.quickThinking

instance RunMessage QuickThinking where
  runMessage msg s@(QuickThinking attrs) = case msg of
    PassedSkillTest iid _ _ SkillTestInitiatorTarget{} _ n | n >= 2 -> s <$ push
      (chooseOne
        iid
        [ Label
          "Take additional action"
          [ CreateEffect
              (toCardCode attrs)
              Nothing
              (toSource attrs)
              (InvestigatorTarget iid)
          ]
        , Label "Pass on additional action" []
        ]
      )
    _ -> QuickThinking <$> runMessage msg attrs
