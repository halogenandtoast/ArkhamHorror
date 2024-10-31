module Arkham.Asset.Assets.CharlesRossEsq (
  charlesRossEsq,
  charlesRossEsqEffect,
  CharlesRossEsq (..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Effect.Import
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Projection
import Arkham.Trait

newtype CharlesRossEsq = CharlesRossEsq AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

charlesRossEsq :: AssetCard CharlesRossEsq
charlesRossEsq = ally CharlesRossEsq Cards.charlesRossEsq (1, 2)

instance HasModifiersFor CharlesRossEsq where
  getModifiersFor (InvestigatorTarget iid) (CharlesRossEsq attrs) | attrs `controlledBy` iid = do
    mlid <- field AssetLocation (toId attrs)
    toModifiers
      attrs
      [ CanSpendResourcesOnCardFromInvestigator
        (investigatorAt lid <> not_ (InvestigatorWithId iid))
        (#asset <> #item)
      | lid <- maybeToList mlid
      ]
  getModifiersFor _ _ = pure []

instance HasAbilities CharlesRossEsq where
  getAbilities (CharlesRossEsq attrs) = [restrictedAbility attrs 1 ControlsThis $ FastAbility $ exhaust attrs]

instance RunMessage CharlesRossEsq where
  runMessage msg a@(CharlesRossEsq attrs) = runQueueT $ case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      -- TODO: we may want to track the investigator instead of the asset
      createCardEffect Cards.charlesRossEsq Nothing source attrs
      pure a
    _ -> CharlesRossEsq <$> liftRunMessage msg attrs

newtype CharlesRossEsqEffect = CharlesRossEsqEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

charlesRossEsqEffect :: EffectArgs -> CharlesRossEsqEffect
charlesRossEsqEffect = cardEffect CharlesRossEsqEffect Cards.charlesRossEsq

instance HasModifiersFor CharlesRossEsqEffect where
  getModifiersFor (InvestigatorTarget iid) (CharlesRossEsqEffect attrs) =
    case attrs.source of
      AssetSource aid -> do
        assetLid <- field AssetLocation aid
        investigatorLid <- field InvestigatorLocation iid
        toModifiers
          attrs
          [ ReduceCostOf (CardWithType AssetType <> CardWithTrait Item) 1
          | isJust assetLid && assetLid == investigatorLid
          ]
      _ -> error "invalid source"
  getModifiersFor _ _ = pure []

instance RunMessage CharlesRossEsqEffect where
  runMessage msg e@(CharlesRossEsqEffect attrs) = runQueueT $ case msg of
    CardEnteredPlay iid card -> case attrs.source of
      AssetSource aid -> do
        assetLid <- field AssetLocation aid
        investigatorLid <- field InvestigatorLocation iid
        when
          (isJust assetLid && assetLid == investigatorLid && cardMatch card (#asset <> CardWithTrait Item))
          (disable attrs)
        pure e
      _ -> error "Invalid source"
    _ -> CharlesRossEsqEffect <$> liftRunMessage msg attrs
