module Arkham.Location.Cards.HuntersLodge (huntersLodge) where

import Arkham.Ability
import Arkham.Helpers.Window (enteringEnemy)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Monster))

newtype HuntersLodge = HuntersLodge LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntersLodge :: LocationCard HuntersLodge
huntersLodge = location HuntersLodge Cards.huntersLodge 4 (PerPlayer 1)

instance HasAbilities HuntersLodge where
  getAbilities (HuntersLodge a) =
    extendRevealed
      a
      [ restricted a 1 Here $ forced $ RevealLocation #after You (be a)
      , mkAbility a 2 $ forced $ EnemyEnters #when (be a) (EnemyWithTrait Monster)
      ]

instance RunMessage HuntersLodge where
  runMessage msg l@(HuntersLodge attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid (attrs.ability 1) iid [#willpower, #intellect] (Fixed 3)
      pure l
    UseCardAbility _ (isSource attrs -> True) 2 (enteringEnemy -> eid) _ -> do
      nonAttackEnemyDamage Nothing (attrs.ability 2) 1 eid 
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignDamage iid (attrs.ability 1) 1
      pure l
    _ -> HuntersLodge <$> liftRunMessage msg attrs
