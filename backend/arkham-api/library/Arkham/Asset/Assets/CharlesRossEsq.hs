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
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified_, modifySelect)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Trait

newtype CharlesRossEsq = CharlesRossEsq AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

charlesRossEsq :: AssetCard CharlesRossEsq
charlesRossEsq = ally CharlesRossEsq Cards.charlesRossEsq (1, 2)

instance HasModifiersFor CharlesRossEsq where
  getModifiersFor (CharlesRossEsq a) = case a.controller of
    Nothing -> pure mempty
    Just iid -> maybeModified_ a iid do
      lid <- MaybeT $ field AssetLocation a.id
      pure
        [ CanSpendResourcesOnCardFromInvestigator
            (investigatorAt lid <> not_ (InvestigatorWithId iid))
            (#asset <> #item)
        ]

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
  getModifiersFor (CharlesRossEsqEffect a) =
    case a.source of
      AssetSource aid ->
        modifySelect a (InvestigatorAt $ locationWithAsset aid) [ReduceCostOf (#asset <> #item) 1]
      _ -> error "invalid source"

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
