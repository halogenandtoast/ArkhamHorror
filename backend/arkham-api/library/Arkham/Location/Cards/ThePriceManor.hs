module Arkham.Location.Cards.ThePriceManor (thePriceManor) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers (ModifierType (Semaphore), semaphore)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Scenarios.TheSecretName.Helpers

newtype ThePriceManor = ThePriceManor LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePriceManor :: LocationCard ThePriceManor
thePriceManor = location ThePriceManor Cards.thePriceManor 2 (PerPlayer 1)

instance HasAbilities ThePriceManor where
  getAbilities (ThePriceManor a) =
    extendRevealed
      a
      [ skillTestAbility $ restricted a 1 Here actionAbility
      , scenarioI18n
          $ withI18nTooltip "thePriceManor.haunted"
          $ restricted a 2 (exists $ IncludeOutOfPlayEnemy $ enemyIs Enemies.nahab) Haunted
      ]

instance RunMessage ThePriceManor where
  runMessage msg l@(ThePriceManor attrs) = runQueueT $ case msg of
    Msg.RevealLocation _ (is attrs -> True) -> do
      ThePriceManor <$> liftRunMessage msg (attrs & labelL .~ "thePriceManor")
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 5)
      pure l
    PassedThisSkillTest _iid (isAbilitySource attrs 1 -> True) -> do
      semaphore attrs do
        gameModifier (attrs.ability 1) attrs Semaphore
        selectOne (IncludeOutOfPlayEnemy $ enemyIs Enemies.nahab)
          >>= traverse_ (removeDoomFrom (attrs.ability 1) 1)
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      selectOne (IncludeOutOfPlayEnemy $ enemyIs Enemies.nahab)
        >>= traverse_ (placeDoomOn (attrs.ability 2) 1)
      pure l
    _ -> ThePriceManor <$> liftRunMessage msg attrs
