module Arkham.Location.Cards.LibraryOfEbla (libraryOfEbla) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers (ModifierType (Semaphore), semaphore)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message qualified as Msg

newtype LibraryOfEbla = LibraryOfEbla LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

libraryOfEbla :: LocationCard LibraryOfEbla
libraryOfEbla = location LibraryOfEbla Cards.libraryOfEbla 4 (PerPlayer 2)

instance HasAbilities LibraryOfEbla where
  getAbilities (LibraryOfEbla a) =
    extendRevealed1 a $ skillTestAbility $ restricted a 1 Here actionAbility

instance RunMessage LibraryOfEbla where
  runMessage msg l@(LibraryOfEbla attrs) = runQueueT $ case msg of
    Msg.RevealLocation _ (is attrs -> True) -> do
      LibraryOfEbla <$> liftRunMessage msg (attrs & labelL .~ "libraryOfEbla")
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 5)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      semaphore attrs do
        gameModifier (attrs.ability 1) attrs Semaphore
        selectOne (IncludeOutOfPlayEnemy $ enemyIs Enemies.nahab)
          >>= traverse_ (nonAttackEnemyDamage (Just iid) (attrs.ability 1) 3)
      pure l
    _ -> LibraryOfEbla <$> liftRunMessage msg attrs
