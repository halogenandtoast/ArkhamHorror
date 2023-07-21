module Arkham.Investigator.Cards.BodyOfAYithian (
  BodyOfAYithian (..),
  YithianMetadata (..),
) where

import Arkham.Prelude

import Arkham.Game.Helpers
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Message

newtype YithianMetadata = YithianMetadata {originalBody :: Value}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype BodyOfAYithian = BodyOfAYithian (InvestigatorAttrs `With` YithianMetadata)
  deriving anyclass (IsInvestigator)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor BodyOfAYithian where
  getModifiersFor (AssetTarget aid) (BodyOfAYithian (a `With` _)) = do
    isYithian <- aid <=~> (assetControlledBy (toId a) <> AllyAsset)
    pure $ toModifiers a [AddTrait Yithian | isYithian]
  getModifiersFor _ _ = pure []

instance HasAbilities BodyOfAYithian where
  getAbilities (BodyOfAYithian _) = []

instance HasChaosTokenValue BodyOfAYithian where
  getChaosTokenValue iid ElderSign (BodyOfAYithian (attrs `With` _))
    | iid == toId attrs = do
        pure $ ChaosTokenValue ElderSign $ PositiveModifier 2
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage BodyOfAYithian where
  runMessage msg i@(BodyOfAYithian (attrs `With` meta)) = case msg of
    ResolveChaosToken _ ElderSign iid | iid == toId attrs -> do
      drawing <- drawCards iid attrs 1
      push drawing
      pure i
    _ -> BodyOfAYithian . (`with` meta) <$> runMessage msg attrs
