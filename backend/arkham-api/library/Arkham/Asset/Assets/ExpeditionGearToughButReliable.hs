module Arkham.Asset.Assets.ExpeditionGearToughButReliable (expeditionGear) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.ForMovement (ForMovement (NotForMovement))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype ExpeditionGearToughButReliable = ExpeditionGearToughButReliable AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

expeditionGear :: AssetCard ExpeditionGearToughButReliable
expeditionGear = asset ExpeditionGearToughButReliable Cards.expeditionGear

instance HasAbilities ExpeditionGearToughButReliable where
  getAbilities (ExpeditionGearToughButReliable a) =
    [ storyControlled
        a
        1
        (exists $ HealableInvestigator (a.ability 1) #damage $ colocatedWithMatch You)
        (actionAbilityWithCost $ assetUseCost a Supply 1)
    , storyControlled
        a
        2
        ( exists
            $ NonEliteEnemy
            <> EnemyAt (orConnected NotForMovement YourLocation)
            <> EnemyCanEnter (ConnectedLocation NotForMovement)
        )
        (FastAbility Free)
    ]

instance RunMessage ExpeditionGearToughButReliable where
  runMessage msg a@(ExpeditionGearToughButReliable attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select $ HealableInvestigator (attrs.ability 1) #damage $ colocatedWith iid
      chooseOrRunOneM iid $ targets investigators \target -> healDamage target (attrs.ability 1) 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      enemies <-
        select
          $ NonEliteEnemy
          <> EnemyAt (orConnected_ $ locationWithInvestigator iid)
          <> EnemyCanEnter (ConnectedLocation NotForMovement)
      chooseTargetM iid enemies \enemy -> do
        destinations <- select $ connectedFrom (locationWithEnemy enemy) <> LocationCanBeEnteredBy enemy
        chooseTargetM iid destinations \destination -> do
          removeFromGame attrs
          enemyMoveTo (attrs.ability 2) enemy destination
      pure a
    _ -> ExpeditionGearToughButReliable <$> liftRunMessage msg attrs
