module Arkham.Event.Cards.TidesOfFate (tidesOfFate, tidesOfFateEffect, TidesOfFate (..)) where

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype TidesOfFate = TidesOfFate EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tidesOfFate :: EventCard TidesOfFate
tidesOfFate = event TidesOfFate Cards.tidesOfFate

instance RunMessage TidesOfFate where
  runMessage msg e@(TidesOfFate attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      tokens <- select $ ChaosTokenFaceIs #curse
      pushAll
        $ replicate (length tokens) (SwapChaosToken #curse #bless)
        <> [createCardEffect Cards.tidesOfFate Nothing attrs iid]
      pure e
    _ -> TidesOfFate <$> runMessage msg attrs

newtype TidesOfFateEffect = TidesOfFateEffect EffectAttrs
  deriving anyclass (HasAbilities, HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tidesOfFateEffect :: EffectArgs -> TidesOfFateEffect
tidesOfFateEffect = cardEffect TidesOfFateEffect Cards.tidesOfFate

instance RunMessage TidesOfFateEffect where
  runMessage msg e@(TidesOfFateEffect attrs) = case msg of
    EndRound -> do
      tokens <- select $ ChaosTokenFaceIs #bless
      pushAll
        $ replicate (length tokens) (SwapChaosToken #bless #curse)
        <> [disable attrs]
      pure e
    _ -> TidesOfFateEffect <$> runMessage msg attrs
