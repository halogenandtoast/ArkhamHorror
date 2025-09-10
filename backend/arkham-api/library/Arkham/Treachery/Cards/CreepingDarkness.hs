module Arkham.Treachery.Cards.CreepingDarkness (creepingDarkness) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CreepingDarkness = CreepingDarkness TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

creepingDarkness :: TreacheryCard CreepingDarkness
creepingDarkness = treachery CreepingDarkness Cards.creepingDarkness

instance HasModifiersFor CreepingDarkness where
  getModifiersFor (CreepingDarkness a) = do
    n <- getPlayerCountValue (PerPlayer 1)
    modifySelect a (enemyIs Enemies.formlessSpawn) [HealthModifier n]

instance HasAbilities CreepingDarkness where
  getAbilities (CreepingDarkness a) = [skillTestAbility $ restricted a 1 OnSameLocation doubleActionAbility]

instance RunMessage CreepingDarkness where
  runMessage msg t@(CreepingDarkness attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      attachTreachery attrs =<< selectJust (locationIs Locations.nexusOfNKai)
      placeDoom attrs attrs 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOrRunOneM iid do
        labeled "Test {willpower} (3)" do
          sid <- getRandom
          beginSkillTest sid iid (attrs.ability 1) attrs #willpower (Fixed 3)
        whenM (getHasSupply iid Torches) do
          labeled "Check supplies" $ toDiscardBy iid (attrs.ability 1) attrs
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    RemoveTreachery tid | tid == attrs.id -> do
      selectEach (enemyIs Enemies.formlessSpawn) $ push . CheckDefeated (attrs.ability 1) . toTarget
      pure t
    _ -> CreepingDarkness <$> liftRunMessage msg attrs
