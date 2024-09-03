module Arkham.Skill.Cards.Defiance (
  defiance,
  defianceEffect,
  Defiance (..),
) where

import Arkham.Prelude

import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Game.Helpers
import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner
import Arkham.Timing qualified as Timing
import Arkham.Window (mkWindow)
import Arkham.Window qualified as Window

newtype Defiance = Defiance SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

defiance :: SkillCard Defiance
defiance = skill Defiance Cards.defiance

instance RunMessage Defiance where
  runMessage msg s@(Defiance attrs) = case msg of
    BeforeRevealChaosTokens -> do
      player <- getPlayer (skillOwner attrs)
      push
        $ chooseOne player
        $ [ Label
              "Choose {skull}"
              [createCardEffect Cards.defiance Nothing (toSource attrs) (ChaosTokenFaceTarget Skull)]
          , Label
              "Choose {cultist}"
              [createCardEffect Cards.defiance Nothing (toSource attrs) (ChaosTokenFaceTarget Cultist)]
          , Label
              "Choose {tablet}"
              [createCardEffect Cards.defiance Nothing (toSource attrs) (ChaosTokenFaceTarget Tablet)]
          , Label
              "Choose {elderThing}"
              [createCardEffect Cards.defiance Nothing (toSource attrs) (ChaosTokenFaceTarget ElderThing)]
          ]
      pure s
    _ -> Defiance <$> runMessage msg attrs

newtype DefianceEffect = DefianceEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

defianceEffect :: EffectArgs -> DefianceEffect
defianceEffect = cardEffect DefianceEffect Cards.defiance

instance HasModifiersFor DefianceEffect where
  getModifiersFor target (DefianceEffect a) | effectTarget a == target = do
    pure $ toModifiers a [IgnoreChaosTokenEffects]
  getModifiersFor _ _ = pure []

instance RunMessage DefianceEffect where
  runMessage msg e@(DefianceEffect attrs@EffectAttrs {..}) = case msg of
    ResolveChaosToken _drawnToken chaosTokenFace _ | not effectFinished && ChaosTokenFaceTarget chaosTokenFace == effectTarget -> do
      ignoreWindow <-
        checkWindows [mkWindow Timing.After (Window.CancelledOrIgnoredCardOrGameEffect $ toSource attrs)]
      push ignoreWindow
      pure $ DefianceEffect $ attrs & finishedL .~ True
    SkillTestEnds _ _ _ -> do
      push (DisableEffect effectId)
      pure e
    _ -> DefianceEffect <$> runMessage msg attrs
