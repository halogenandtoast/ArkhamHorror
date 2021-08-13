module Arkham.Types.Skill.Cards.Defiance
  ( defiance
  , Defiance(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Skill.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Runner
import Arkham.Types.Target
import Arkham.Types.Token

newtype Defiance = Defiance SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor env, HasActions)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

defiance :: SkillCard Defiance
defiance = skill Defiance Cards.defiance

instance SkillRunner env => RunMessage env Defiance where
  runMessage msg s@(Defiance attrs) = case msg of
    BeforeRevealTokens -> do
      s <$ push
        (chooseOne
          (skillOwner attrs)
          [ Label
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
        )
    _ -> Defiance <$> runMessage msg attrs
