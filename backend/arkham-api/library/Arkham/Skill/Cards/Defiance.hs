module Arkham.Skill.Cards.Defiance (defiance, defianceEffect, Defiance (..)) where

import Arkham.ChaosToken
import Arkham.Effect.Import
import Arkham.Game.Helpers
import Arkham.Message.Lifted.Choose
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Defiance = Defiance SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

defiance :: SkillCard Defiance
defiance = skill Defiance Cards.defiance

instance RunMessage Defiance where
  runMessage msg s@(Defiance attrs) = runQueueT $ case msg of
    BeforeRevealChaosTokens -> do
      chooseOneM (skillOwner attrs) do
        labeled "Choose {skull}"
          $ createCardEffect Cards.defiance Nothing (toSource attrs) (ChaosTokenFaceTarget Skull)
        labeled "Choose {cultist}"
          $ createCardEffect Cards.defiance Nothing (toSource attrs) (ChaosTokenFaceTarget Cultist)
        labeled "Choose {tablet}"
          $ createCardEffect Cards.defiance Nothing (toSource attrs) (ChaosTokenFaceTarget Tablet)
        labeled "Choose {elderThing}"
          $ createCardEffect Cards.defiance Nothing (toSource attrs) (ChaosTokenFaceTarget ElderThing)
      pure s
    _ -> Defiance <$> liftRunMessage msg attrs

newtype DefianceEffect = DefianceEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

defianceEffect :: EffectArgs -> DefianceEffect
defianceEffect = cardEffect DefianceEffect Cards.defiance

instance HasModifiersFor DefianceEffect where
  getModifiersFor target (DefianceEffect a) | a.target == target = do
    toModifiers a [IgnoreChaosTokenEffects]
  getModifiersFor _ _ = pure []

instance RunMessage DefianceEffect where
  runMessage msg e@(DefianceEffect attrs) = runQueueT $ case msg of
    ResolveChaosToken _drawnToken chaosTokenFace _ | not attrs.finished && ChaosTokenFaceTarget chaosTokenFace == attrs.target -> do
      cancelledOrIgnoredCardOrGameEffect attrs.source
      pure $ DefianceEffect $ finishedEffect attrs
    SkillTestEnds _ _ _ -> disableReturn e
    _ -> DefianceEffect <$> liftRunMessage msg attrs
