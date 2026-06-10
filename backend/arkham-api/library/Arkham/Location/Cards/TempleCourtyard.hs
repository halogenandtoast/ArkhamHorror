module Arkham.Location.Cards.TempleCourtyard (templeCourtyard) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectWhen)
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey

newtype TempleCourtyard = TempleCourtyard LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

templeCourtyard :: LocationCard TempleCourtyard
templeCourtyard = location TempleCourtyard Cards.templeCourtyard 3 (PerPlayer 1)

instance HasModifiersFor TempleCourtyard where
  getModifiersFor (TempleCourtyard a) = do
    modifySelectWhen
      a
      (a.clues == 0)
      (enemyIs Enemies.humbleSupplicant)
      [RemoveKeyword Keyword.Aloof]

instance HasAbilities TempleCourtyard where
  getAbilities (TempleCourtyard a) =
    extendRevealed1 a $ skillTestAbility $ restricted a 1 Here actionAbility

instance RunMessage TempleCourtyard where
  runMessage msg l@(TempleCourtyard attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #combat (Fixed 5)
      pure l
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      remember BrokenIntoADesertedTemple
      pure l
    _ -> TempleCourtyard <$> liftRunMessage msg attrs
