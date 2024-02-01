module Arkham.Effect.Effects.GildedVolto (
  GildedVolto (..),
  gildedVolto,
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype GildedVolto = GildedVolto EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

gildedVolto :: EffectArgs -> GildedVolto
gildedVolto = GildedVolto . uncurry4 (baseAttrs "82026")

instance HasModifiersFor GildedVolto where
  getModifiersFor target (GildedVolto a)
    | target == effectTarget a =
        pure [toModifier a $ CanBecomeFast $ CardWithType AssetType]
  getModifiersFor (CardTarget card) (GildedVolto a)
    | (InvestigatorTarget <$> toCardOwner card) == Just (effectTarget a) =
        pure
          [toModifier a BecomesFast | cardMatch card (CardWithType AssetType)]
  getModifiersFor _ _ = pure []

instance RunMessage GildedVolto where
  runMessage msg e@(GildedVolto attrs) = case msg of
    CardEnteredPlay iid card
      | InvestigatorTarget iid
          == effectTarget attrs
          && cardMatch
            card
            (CardWithType AssetType) ->
          e <$ push (DisableEffect $ toId attrs)
    EndTurn _ -> e <$ push (DisableEffect $ toId attrs)
    _ -> GildedVolto <$> runMessage msg attrs
