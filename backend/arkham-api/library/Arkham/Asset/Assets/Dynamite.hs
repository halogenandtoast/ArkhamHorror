module Arkham.Asset.Assets.Dynamite (dynamite) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), withoutModifier)
import Arkham.Matcher

newtype Dynamite = Dynamite AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dynamite :: AssetCard Dynamite
dynamite = asset Dynamite Cards.dynamite

instance HasAbilities Dynamite where
  getAbilities (Dynamite a) =
    [ storyControlled a 1 CanDealDamage
        $ actionAbilityWithCost (exhaust a <> assetUseCost a Supply 1)
    ]

instance RunMessage Dynamite where
  runMessage msg a@(Dynamite attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- select $ orConnected (locationWithInvestigator iid)
      canDealDamage <- withoutModifier iid CannotDealDamage
      chooseOneM iid do
        for_ locations \location -> do
          enemies <- if canDealDamage then select (enemyAt location) else pure []
          investigators <- select $ investigatorAt location
          unless (null enemies && null investigators) do
            targeting location do
              uiEffect attrs location Explosion
              for_ enemies (nonAttackEnemyDamage attrs 3)
              for_ investigators \iid' -> assignDamage iid' attrs 3
      pure a
    _ -> Dynamite <$> liftRunMessage msg attrs
