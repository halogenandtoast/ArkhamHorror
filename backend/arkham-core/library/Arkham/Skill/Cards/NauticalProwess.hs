module Arkham.Skill.Cards.NauticalProwess (
  nauticalProwess,
  NauticalProwess (..),
)
where

import Arkham.Card
import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Matcher hiding (RevealChaosToken)
import Arkham.Prelude
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype NauticalProwess = NauticalProwess SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nauticalProwess :: SkillCard NauticalProwess
nauticalProwess =
  skillWith NauticalProwess Cards.nauticalProwess (setMeta @Bool True)

instance RunMessage NauticalProwess where
  runMessage msg s@(NauticalProwess attrs) = case msg of
    RevealChaosToken _ _ token -> do
      let meta = toResult @Bool attrs.meta
      isNegative <- token <=~> WithNegativeModifier
      if meta && isNegative
        then do
          mDrawing <- drawCardsIfCan attrs.owner attrs 1
          player <- getPlayer attrs.owner
          push
            $ chooseOrRunOne player
            $ [ Label
                  "Nautical Prowess gains {wild}{wild}"
                  [ skillTestModifier attrs (CardIdTarget $ toCardId attrs) $ AddSkillIcons [#wild, #wild]
                  ]
              ]
            <> [Label "Draw 1 card" [drawing] | drawing <- toList mDrawing]
          pure . NauticalProwess $ attrs & setMeta @Bool True
        else pure s
    _ -> NauticalProwess <$> runMessage msg attrs
