module Arkham.Effect.Effects.CharlesRossEsq (
  CharlesRossEsq (..),
  charlesRossEsq,
) where

import Arkham.Prelude

import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Game.Helpers
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Trait

newtype CharlesRossEsq = CharlesRossEsq EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

charlesRossEsq :: EffectArgs -> CharlesRossEsq
charlesRossEsq = CharlesRossEsq . uncurry4 (baseAttrs "03149")

instance HasModifiersFor CharlesRossEsq where
  getModifiersFor (InvestigatorTarget iid) (CharlesRossEsq attrs) =
    case effectSource attrs of
      AssetSource aid -> do
        assetLid <- field AssetLocation aid
        investigatorLid <- field InvestigatorLocation iid
        pure $
          toModifiers
            attrs
            [ ReduceCostOf (CardWithType AssetType <> CardWithTrait Item) 1
            | isJust assetLid && assetLid == investigatorLid
            ]
      _ -> error "invalid source"
  getModifiersFor _ _ = pure []

instance RunMessage CharlesRossEsq where
  runMessage msg e@(CharlesRossEsq attrs) = case msg of
    CardEnteredPlay iid card -> case effectSource attrs of
      AssetSource aid -> do
        assetLid <- field AssetLocation aid
        investigatorLid <- field InvestigatorLocation iid
        if isJust assetLid
          && assetLid
          == investigatorLid
          && cardMatch
            card
            (CardWithType AssetType <> CardWithTrait Item)
          then e <$ push (DisableEffect $ toId attrs)
          else pure e
      _ -> error "Invalid source"
    _ -> CharlesRossEsq <$> runMessage msg attrs
