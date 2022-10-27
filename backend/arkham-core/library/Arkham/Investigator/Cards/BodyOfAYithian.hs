module Arkham.Investigator.Cards.BodyOfAYithian
  ( BodyOfAYithian(..)
  , YithianMetadata(..)
  ) where

import Arkham.Prelude

import Arkham.Game.Helpers
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Target

newtype YithianMetadata = YithianMetadata { original :: Value }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype BodyOfAYithian = BodyOfAYithian (InvestigatorAttrs `With` YithianMetadata)
  deriving anyclass IsInvestigator
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor BodyOfAYithian where
  getModifiersFor (AssetTarget aid) (BodyOfAYithian (a `With` _)) = do
    isYithian <- aid <=~> (assetControlledBy (toId a) <> AllyAsset)
    pure $ toModifiers a [ AddTrait Yithian | isYithian ]
  getModifiersFor _ _ = pure []

instance HasAbilities BodyOfAYithian where
  getAbilities (BodyOfAYithian _) = []

instance HasTokenValue BodyOfAYithian where
  getTokenValue iid ElderSign (BodyOfAYithian (attrs `With` _))
    | iid == toId attrs = do
      pure $ TokenValue ElderSign $ PositiveModifier 2
  getTokenValue _ token _ = pure $ TokenValue token mempty

instance RunMessage BodyOfAYithian where
  runMessage msg i@(BodyOfAYithian (attrs `With` meta)) = case msg of
    ResolveToken _ ElderSign iid | iid == toId attrs -> do
      push $ DrawCards iid 1 False
      pure i
    _ -> BodyOfAYithian . (`with` meta) <$> runMessage msg attrs
