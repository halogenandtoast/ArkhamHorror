module Arkham.Skill.Cards.Defiance2
  ( defiance2
  , Defiance2(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner
import Arkham.Target
import Arkham.Token

newtype Defiance2 = Defiance2 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

defiance2 :: SkillCard Defiance2
defiance2 = skill Defiance2 Cards.defiance2

instance RunMessage Defiance2 where
  runMessage msg s@(Defiance2 attrs) = case msg of
    InvestigatorCommittedSkill _ sid | sid == toId attrs -> do
      pushAll
        [ skillTestModifier
            (toSource attrs)
            (TokenFaceTarget t)
            IgnoreTokenEffects
        | t <- [Skull, Cultist, Tablet, ElderThing]
        ]
      pure s
    _ -> Defiance2 <$> runMessage msg attrs
