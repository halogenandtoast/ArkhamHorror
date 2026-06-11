module Arkham.Asset.Assets.HungerDiagram (hungerDiagram) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.DamageEffect (nonAttack)
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

-- While you are in the Chamber of Hunger, it gains: "[action] If Eixodolon's
-- Pet is 'locked away,' spend 1 clue: Deal 4 damage to Eixodolon's Pet."
instance HasAbilities HungerDiagram where
  getAbilities (HungerDiagram a) =
    [ restricted a 1 (ControlsThis <> exists (NotInvestigator You))
        $ forced
        $ Matcher.InvestigatorDefeated #when ByAny You
    , restricted
        a
        2
        ( ControlsThis
            <> youExist (InvestigatorAt $ locationIs Locations.chamberOfHunger)
            <> exists (enemyIs Enemies.eixodolonsPet <> EnemyWithPlacement Global)
        )
        $ actionAbilityWithCost (ClueCost (Static 1))
    ]

instance RunMessage HungerDiagram where
  runMessage msg a@(HungerDiagram attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select $ NotInvestigator $ InvestigatorWithId iid
      chooseOrRunOneM iid do
        targets investigators (`takeControlOfAsset` attrs.id)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      pet <- selectJust $ enemyIs Enemies.eixodolonsPet
      push $ DealDamage (EnemyTarget pet) (nonAttack Nothing (attrs.ability 2) 4)
      pure a
    _ -> HungerDiagram <$> liftRunMessage msg attrs
