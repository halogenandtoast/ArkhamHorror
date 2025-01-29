module Arkham.Asset.Assets.CharlesRossEsq (charlesRossEsq, charlesRossEsqEffect) where

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
  getModifiersFor (CharlesRossEsq a) = for_ a.controller \iid -> do
    maybeModified_ a iid do
      lid <- MaybeT $ field AssetLocation a.id
      pure
        [ CanSpendResourcesOnCardFromInvestigator
            (investigatorAt lid <> not_ (be iid))
            (#asset <> #item)
        ]

instance HasAbilities CharlesRossEsq where
  getAbilities (CharlesRossEsq attrs) = [restrictedAbility attrs 1 ControlsThis $ FastAbility $ exhaust attrs]

instance RunMessage CharlesRossEsq where
  runMessage msg a@(CharlesRossEsq attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      createCardEffect Cards.charlesRossEsq Nothing (attrs.ability 1) iid
      pure a
    _ -> CharlesRossEsq <$> liftRunMessage msg attrs

newtype CharlesRossEsqEffect = CharlesRossEsqEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

charlesRossEsqEffect :: EffectArgs -> CharlesRossEsqEffect
charlesRossEsqEffect = cardEffect CharlesRossEsqEffect Cards.charlesRossEsq

instance HasModifiersFor CharlesRossEsqEffect where
  getModifiersFor (CharlesRossEsqEffect a) = for_ a.target.investigator \iid -> do
    modifySelect a (colocatedWith iid) [ReduceCostOf (#asset <> #item) 1]

instance RunMessage CharlesRossEsqEffect where
  runMessage msg e@(CharlesRossEsqEffect attrs) = runQueueT $ case msg of
    CardEnteredPlay iid card -> do
      for_ attrs.target.investigator \iid' -> void $ runMaybeT do
        guard $ cardMatch card (#asset <> CardWithTrait Item)
        yourLocation <- MaybeT $ field InvestigatorLocation iid'
        investigatorLid <- MaybeT $ field InvestigatorLocation iid
        guard $ yourLocation == investigatorLid
        lift $ disable attrs
      pure e
    _ -> CharlesRossEsqEffect <$> liftRunMessage msg attrs
