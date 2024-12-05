module Arkham.Asset.Assets.TheBlackCat5 (theBlackCat5, TheBlackCat5 (..)) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.ChaosToken
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Helpers.SkillTest (withSkillTest)

newtype TheBlackCat5 = TheBlackCat5 AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBlackCat5 :: AssetCard TheBlackCat5
theBlackCat5 = ally TheBlackCat5 Cards.theBlackCat5 (3, 3)

instance HasModifiersFor TheBlackCat5 where
  getModifiersFor (TheBlackCat5 a) =
    controllerGets
      a
      [ CanResolveToken #tablet (toTarget a)
      , CanResolveToken #elderthing (toTarget a)
      , CanResolveToken #eldersign (toTarget a)
      ]

instance RunMessage TheBlackCat5 where
  runMessage msg a@(TheBlackCat5 attrs) = runQueueT $ case msg of
    TargetResolveChaosToken (isTarget attrs -> True) token Tablet _ -> do
      withSkillTest \sid -> do
        skillTestModifier sid attrs token (ChangeChaosTokenModifier $ NegativeModifier 1)
        push $ DealAssetDirectDamage (toId attrs) (ChaosTokenEffectSource Tablet) 1 0
      pure a
    TargetResolveChaosToken (isTarget attrs -> True) token ElderThing _ -> do
      withSkillTest \sid -> do
        skillTestModifier sid attrs token (ChangeChaosTokenModifier $ NegativeModifier 1)
        push $ DealAssetDirectDamage (toId attrs) (ChaosTokenEffectSource Tablet) 0 1
      pure a
    TargetResolveChaosToken (isTarget attrs -> True) token ElderSign _ -> do
      withSkillTest \sid -> do
        skillTestModifier sid attrs token (ChangeChaosTokenModifier $ PositiveModifier 5)
        push $ HealAllDamage (toTarget attrs) (ChaosTokenEffectSource ElderSign)
        push $ HealAllHorror (toTarget attrs) (ChaosTokenEffectSource ElderSign)
      pure a
    _ -> TheBlackCat5 <$> liftRunMessage msg attrs
