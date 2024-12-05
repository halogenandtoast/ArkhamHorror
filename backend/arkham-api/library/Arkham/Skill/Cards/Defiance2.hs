module Arkham.Skill.Cards.Defiance2 (defiance2, defiance2Effect, Defiance2 (..)) where

import Arkham.ChaosToken
import Arkham.Effect.Import
import Arkham.Game.Helpers
import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Defiance2 = Defiance2 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

defiance2 :: SkillCard Defiance2
defiance2 = skill Defiance2 Cards.defiance2

instance RunMessage Defiance2 where
  runMessage msg (Defiance2 attrs) = runQueueT $ case msg of
    InvestigatorCommittedSkill _ sid | sid == attrs.id -> do
      createCardEffect Cards.defiance2 Nothing attrs attrs
      Defiance2 <$> liftRunMessage msg attrs
    _ -> Defiance2 <$> liftRunMessage msg attrs

newtype Defiance2Effect = Defiance2Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

defiance2Effect :: EffectArgs -> Defiance2Effect
defiance2Effect = cardEffect Defiance2Effect Cards.defiance2

instance HasModifiersFor Defiance2Effect where
  getModifiersFor (Defiance2Effect a) =
    modifyEach
      a
      (map ChaosTokenFaceTarget [Skull, Cultist, Tablet, ElderThing])
      [IgnoreChaosTokenEffects]

instance RunMessage Defiance2Effect where
  runMessage msg e@(Defiance2Effect attrs) = runQueueT $ case msg of
    ResolveChaosToken _drawnToken chaosTokenFace _ | not attrs.finished && chaosTokenFace `elem` [Skull, Cultist, Tablet, ElderThing] -> do
      cancelledOrIgnoredCardOrGameEffect attrs.source
      pure $ Defiance2Effect $ finishedEffect attrs
    SkillTestEnds _ _ _ -> disableReturn e
    _ -> Defiance2Effect <$> liftRunMessage msg attrs
