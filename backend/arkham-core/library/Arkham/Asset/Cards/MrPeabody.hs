module Arkham.Asset.Cards.MrPeabody (mrPeabody, mrPeabodyEffect, MrPeabody (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Effect.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Trait (Trait (Passageway))

newtype MrPeabody = MrPeabody AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mrPeabody :: AssetCard MrPeabody
mrPeabody = ally MrPeabody Cards.mrPeabody (2, 2)

instance HasAbilities MrPeabody where
  getAbilities (MrPeabody attrs) = [restrictedAbility attrs 1 ControlsThis $ actionAbilityWithCost (exhaust attrs)]

instance RunMessage MrPeabody where
  runMessage msg a@(MrPeabody attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      locations <- selectList Anywhere
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ targetLabel target [createCardEffect Cards.mrPeabody Nothing source target]
          | target <- locations
          ]
      pure a
    _ -> MrPeabody <$> runMessage msg attrs

newtype MrPeabodyEffect = MrPeabodyEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mrPeabodyEffect :: EffectArgs -> MrPeabodyEffect
mrPeabodyEffect = cardEffect MrPeabodyEffect Cards.mrPeabody

instance HasModifiersFor MrPeabodyEffect where
  getModifiersFor target (MrPeabodyEffect attrs) | attrs.target == target = do
    pure $ toModifiers attrs [ShroudModifier (-1), AddTrait Passageway]
  getModifiersFor _ _ = pure []

instance RunMessage MrPeabodyEffect where
  runMessage msg e@(MrPeabodyEffect attrs) = case msg of
    Ready (AssetTarget aid) | AssetSource aid == effectSource attrs -> do
      push $ disable attrs
      pure e
    _ -> MrPeabodyEffect <$> runMessage msg attrs
