module Arkham.Location.Cards.WaterTower (waterTower) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.TheBlobThatAteEverything.Helpers
import Arkham.Trait (Trait (Ooze))

newtype WaterTower = WaterTower LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

waterTower :: LocationCard WaterTower
waterTower = locationWith WaterTower Cards.waterTower 4 (PerPlayer 1) connectsToAdjacent

instance HasAbilities WaterTower where
  getAbilities (WaterTower a) =
    let usedSuccess = toResultDefault False a.meta
     in extendRevealed a
          $ [ scenarioI18n
              $ withI18nTooltip "waterTower.damage"
              $ skillTestAbility
              $ restricted a 1 (Here <> thisExists a LocationWithoutClues) actionAbility
            | not usedSuccess
            ]

instance RunMessage WaterTower where
  runMessage msg l@(WaterTower attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #intellect (Fixed 3)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      oozes <-
        select
          $ EnemyWithTrait Ooze
          <> EnemyAt (oneOf [be attrs, connectedFrom (be attrs)])
      for_ oozes $ nonAttackEnemyDamage_ (Just iid) (attrs.ability 1) 3
      pure $ WaterTower $ setMeta True attrs
    _ -> WaterTower <$> liftRunMessage msg attrs
