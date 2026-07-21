module Arkham.Asset.Assets.HungerDiagram (hungerDiagram) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement

newtype HungerDiagram = HungerDiagram AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hungerDiagram :: AssetCard HungerDiagram
hungerDiagram = asset HungerDiagram Cards.hungerDiagram

instance HasAbilities HungerDiagram where
  getAbilities (HungerDiagram a) =
    [ controlled a 1 (exists $ not_ You) $ forced $ Matcher.InvestigatorDefeated #when ByAny You
    , restricted
        (proxied (locationIs Locations.chamberOfHunger) a)
        2
        ( exists (at_ (locationIs Locations.chamberOfHunger) <> HasMatchingAsset (be a))
            <> exists (enemyIs Enemies.eixodolonsPet <> EnemyWithPlacement Global)
        )
        $ actionAbilityWithCost (ClueCost (Static 1))
    ]

instance RunMessage HungerDiagram where
  runMessage msg a@(HungerDiagram attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select $ not_ $ InvestigatorWithId iid
      chooseOrRunOneM iid $ targets investigators (`takeControlOfAsset` attrs.id)
      pure a
    UseThisAbility _ source@(isProxySource attrs -> True) 2 -> do
      pet <- selectJust $ enemyIs Enemies.eixodolonsPet
      nonAttackEnemyDamage_ Nothing source 4 pet
      pure a
    _ -> HungerDiagram <$> liftRunMessage msg attrs
