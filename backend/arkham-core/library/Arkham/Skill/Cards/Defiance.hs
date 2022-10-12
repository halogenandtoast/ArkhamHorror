module Arkham.Skill.Cards.Defiance
  ( defiance
  , Defiance(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner
import Arkham.Target
import Arkham.Token

newtype Defiance = Defiance SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

defiance :: SkillCard Defiance
defiance = skill Defiance Cards.defiance

instance RunMessage Defiance where
  runMessage msg s@(Defiance attrs) = case msg of
    BeforeRevealTokens -> do
      push
        $ chooseOne (skillOwner attrs)
        $ [ Label
            "Choose {skull}"
            [ skillTestModifier
                (toSource attrs)
                (TokenFaceTarget Skull)
                IgnoreTokenEffects
            ]
          , Label
            "Choose {cultist}"
            [ skillTestModifier
                (toSource attrs)
                (TokenFaceTarget Cultist)
                IgnoreTokenEffects
            ]
          , Label
            "Choose {tablet}"
            [ skillTestModifier
                (toSource attrs)
                (TokenFaceTarget Tablet)
                IgnoreTokenEffects
            ]
          , Label
            "Choose {elderThing}"
            [ skillTestModifier
                (toSource attrs)
                (TokenFaceTarget ElderThing)
                IgnoreTokenEffects
            ]
          ]
      pure s
    _ -> Defiance <$> runMessage msg attrs
