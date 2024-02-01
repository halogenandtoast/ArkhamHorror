module Arkham.Asset.Cards.CharlesRossEsq (
  charlesRossEsq,
  charlesRossEsqEffect,
  CharlesRossEsq (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Effect.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Trait

newtype CharlesRossEsq = CharlesRossEsq AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

charlesRossEsq :: AssetCard CharlesRossEsq
charlesRossEsq = ally CharlesRossEsq Cards.charlesRossEsq (1, 2)

instance HasModifiersFor CharlesRossEsq where
  getModifiersFor (InvestigatorTarget iid) (CharlesRossEsq attrs) | attrs `controlledBy` iid = do
    mlid <- field AssetLocation (toId attrs)
    pure
      $ toModifiers
        attrs
        [ CanSpendResourcesOnCardFromInvestigator (investigatorAt lid) (#asset <> #item)
        | lid <- maybeToList mlid
        ]
  getModifiersFor _ _ = pure []

instance HasAbilities CharlesRossEsq where
  getAbilities (CharlesRossEsq attrs) = [restrictedAbility attrs 1 ControlsThis $ FastAbility $ exhaust attrs]

instance RunMessage CharlesRossEsq where
  runMessage msg a@(CharlesRossEsq attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      -- TODO: we may want to track the investigator instead of the asset
      push $ createCardEffect Cards.charlesRossEsq Nothing source attrs
      pure a
    _ -> CharlesRossEsq <$> runMessage msg attrs

newtype CharlesRossEsqEffect = CharlesRossEsqEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

charlesRossEsqEffect :: EffectArgs -> CharlesRossEsqEffect
charlesRossEsqEffect = cardEffect CharlesRossEsqEffect Cards.charlesRossEsq

instance HasModifiersFor CharlesRossEsqEffect where
  getModifiersFor (InvestigatorTarget iid) (CharlesRossEsqEffect attrs) =
    case effectSource attrs of
      AssetSource aid -> do
        assetLid <- field AssetLocation aid
        investigatorLid <- field InvestigatorLocation iid
        pure
          $ toModifiers
            attrs
            [ ReduceCostOf (CardWithType AssetType <> CardWithTrait Item) 1
            | isJust assetLid && assetLid == investigatorLid
            ]
      _ -> error "invalid source"
  getModifiersFor _ _ = pure []

instance RunMessage CharlesRossEsqEffect where
  runMessage msg e@(CharlesRossEsqEffect attrs) = case msg of
    CardEnteredPlay iid card -> case effectSource attrs of
      AssetSource aid -> do
        assetLid <- field AssetLocation aid
        investigatorLid <- field InvestigatorLocation iid
        pushWhen
          (isJust assetLid && assetLid == investigatorLid && cardMatch card (#asset <> CardWithTrait Item))
          (disable attrs)
        pure e
      _ -> error "Invalid source"
    _ -> CharlesRossEsqEffect <$> runMessage msg attrs
