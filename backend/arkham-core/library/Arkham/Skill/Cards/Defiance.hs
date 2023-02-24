module Arkham.Skill.Cards.Defiance
  ( defiance
  , defianceEffect
  , Defiance(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Message
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Window (Window(..))
import Arkham.Window qualified as Window

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
              [ createCardEffect Cards.defiance Nothing (toSource attrs) (TokenFaceTarget Skull) ]
            , Label
              "Choose {cultist}"
              [ createCardEffect Cards.defiance Nothing (toSource attrs) (TokenFaceTarget Cultist) ]
            , Label
              "Choose {tablet}"
              [ createCardEffect Cards.defiance Nothing (toSource attrs) (TokenFaceTarget Tablet) ]
            , Label
              "Choose {elderThing}"
              [ createCardEffect Cards.defiance Nothing (toSource attrs) (TokenFaceTarget ElderThing) ]
            ]
      pure s
    _ -> Defiance <$> runMessage msg attrs

newtype DefianceEffect = DefianceEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

defianceEffect :: EffectArgs -> DefianceEffect
defianceEffect = cardEffect DefianceEffect Cards.defiance

instance HasModifiersFor DefianceEffect where
  getModifiersFor target (DefianceEffect a) | effectTarget a == target =
    pure $ toModifiers a [ IgnoreTokenEffects ]
  getModifiersFor _ _ = pure []

instance RunMessage DefianceEffect where
  runMessage msg e@(DefianceEffect attrs@EffectAttrs {..}) = case msg of
    ResolveToken _drawnToken tokenFace _ | not effectFinished && TokenFaceTarget tokenFace == effectTarget -> do
      ignoreWindow <- checkWindows [Window Timing.After (Window.CancelledOrIgnoredCardOrGameEffect $ toSource attrs)]
      push ignoreWindow
      pure $ DefianceEffect $ attrs & finishedL .~ True
    SkillTestEnds _ _ -> do
      push (DisableEffect effectId)
      pure e
    _ -> DefianceEffect <$> runMessage msg attrs
