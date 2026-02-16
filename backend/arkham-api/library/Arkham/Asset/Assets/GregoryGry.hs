module Arkham.Asset.Assets.GregoryGry (gregoryGry, gregoryGryEffect) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Capability
import Arkham.Effect.Import
import Arkham.Matcher

newtype GregoryGry = GregoryGry AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gregoryGry :: AssetCard GregoryGry
gregoryGry = ally GregoryGry Cards.gregoryGry (1, 2)

instance HasAbilities GregoryGry where
  getAbilities (GregoryGry a) =
    [ controlled_ a 1
        $ triggered
          (InitiatedSkillTest #when You AnySkillType AnySkillTestValue #any)
          (UseCostUpTo (be a) Resource 1 3)
    ]

instance RunMessage GregoryGry where
  runMessage msg a@(GregoryGry attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (totalUsesPayment -> uses) -> do
      createCardEffect Cards.gregoryGry (effectInt uses) (attrs.ability 1) iid
      pure a
    _ -> GregoryGry <$> liftRunMessage msg attrs

newtype GregoryGryEffect = GregoryGryEffect EffectAttrs
  deriving anyclass (HasAbilities, HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gregoryGryEffect :: EffectArgs -> GregoryGryEffect
gregoryGryEffect = cardEffect GregoryGryEffect Cards.gregoryGry

instance RunMessage GregoryGryEffect where
  runMessage msg e@(GregoryGryEffect attrs) = runQueueT $ case msg of
    PassedSkillTest _ _ _ _ _ x -> do
      case attrs.metaInt of
        Just n -> do
          when (x >= n) do
            let iid = fromJustNote "Wrong Type" attrs.target.investigator
            whenMatch iid (investigator_ can.gain.resources) do
              skillTestResultOption "Gregory Gry" $ gainResources iid attrs.source n
          disable attrs
        _ -> error "Wrong metadata"
      pure e
    SkillTestEnds {} -> disableReturn e
    _ -> GregoryGryEffect <$> liftRunMessage msg attrs
