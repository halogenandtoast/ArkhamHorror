module Arkham.Skill.Cards.Defiance2 (
  defiance2,
  defiance2Effect,
  Defiance2 (..),
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

newtype Defiance2 = Defiance2 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

defiance2 :: SkillCard Defiance2
defiance2 = skill Defiance2 Cards.defiance2

instance RunMessage Defiance2 where
  runMessage msg s@(Defiance2 attrs) = case msg of
    InvestigatorCommittedSkill _ sid | sid == toId attrs -> do
      push $ createCardEffect Cards.defiance2 Nothing (toSource attrs) (toTarget attrs)
      pure s
    _ -> Defiance2 <$> runMessage msg attrs

newtype Defiance2Effect = Defiance2Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

defiance2Effect :: EffectArgs -> Defiance2Effect
defiance2Effect = cardEffect Defiance2Effect Cards.defiance2

instance HasModifiersFor Defiance2Effect where
  getModifiersFor (ChaosTokenFaceTarget face) (Defiance2Effect a)
    | face `elem` [Skull, Cultist, Tablet, ElderThing] =
        pure $ toModifiers a [IgnoreChaosTokenEffects]
  getModifiersFor _ _ = pure []

instance RunMessage Defiance2Effect where
  runMessage msg e@(Defiance2Effect attrs@EffectAttrs {..}) = case msg of
    ResolveChaosToken _drawnToken chaosTokenFace _ | not effectFinished && chaosTokenFace `elem` [Skull, Cultist, Tablet, ElderThing] -> do
      ignoreWindow <-
        checkWindows [mkWindow Timing.After (Window.CancelledOrIgnoredCardOrGameEffect $ toSource attrs)]
      push ignoreWindow
      pure $ Defiance2Effect $ attrs & finishedL .~ True
    SkillTestEnds _ _ -> do
      push (DisableEffect effectId)
      pure e
    _ -> Defiance2Effect <$> runMessage msg attrs
