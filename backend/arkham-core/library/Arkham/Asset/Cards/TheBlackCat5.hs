module Arkham.Asset.Cards.TheBlackCat5 (
  theBlackCat5,
  TheBlackCat5 (..),
)
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosToken

newtype TheBlackCat5 = TheBlackCat5 AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBlackCat5 :: AssetCard TheBlackCat5
theBlackCat5 = ally TheBlackCat5 Cards.theBlackCat5 (3, 3)

instance HasModifiersFor TheBlackCat5 where
  getModifiersFor (InvestigatorTarget iid) (TheBlackCat5 a) | a `controlledBy` iid = do
    pure
      $ toModifiers
        a
        [ CanResolveToken #tablet (toTarget a)
        , CanResolveToken #elderthing (toTarget a)
        , CanResolveToken #eldersign (toTarget a)
        ]
  getModifiersFor _ _ = pure []

instance RunMessage TheBlackCat5 where
  runMessage msg a@(TheBlackCat5 attrs) = case msg of
    TargetResolveChaosToken (isTarget attrs -> True) token Tablet _ -> do
      pushAll
        [ skillTestModifier attrs token (ChangeChaosTokenModifier $ NegativeModifier 1)
        , AssetDamageWithCheck (toId attrs) (ChaosTokenEffectSource Tablet) 1 0 True
        ]
      pure a
    TargetResolveChaosToken (isTarget attrs -> True) token ElderThing _ -> do
      pushAll
        [ skillTestModifier attrs token (ChangeChaosTokenModifier $ NegativeModifier 1)
        , AssetDamageWithCheck (toId attrs) (ChaosTokenEffectSource Tablet) 0 1 True
        ]
      pure a
    TargetResolveChaosToken (isTarget attrs -> True) token ElderSign _ -> do
      pushAll
        [ skillTestModifier attrs token (ChangeChaosTokenModifier $ PositiveModifier 5)
        , HealAllDamage (toTarget attrs) (ChaosTokenEffectSource ElderSign)
        , HealAllHorror (toTarget attrs) (ChaosTokenEffectSource ElderSign)
        ]
      pure a
    _ -> TheBlackCat5 <$> runMessage msg attrs
