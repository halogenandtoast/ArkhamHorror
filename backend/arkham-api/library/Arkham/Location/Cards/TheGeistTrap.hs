module Arkham.Location.Cards.TheGeistTrap (theGeistTrap, TheGeistTrap (..)) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.UnionAndDisillusion.Helpers

newtype TheGeistTrap = TheGeistTrap LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGeistTrap :: LocationCard TheGeistTrap
theGeistTrap = location TheGeistTrap Cards.theGeistTrap 4 (PerPlayer 1)

instance HasModifiersFor TheGeistTrap where
  getModifiersFor target (TheGeistTrap attrs)
    | attrs `isTarget` target
    , not attrs.revealed = do
        toModifiers attrs [Blocked]
  getModifiersFor (EnemyTarget eid) (TheGeistTrap attrs) | attrs.revealed = do
    gainsRetaliate <- selectAny $ be eid <> enemyIs Enemies.theSpectralWatcher <> at_ (be attrs)
    toModifiers attrs [AddKeyword Keyword.Retaliate | gainsRetaliate]
  getModifiersFor _ _ = pure []

instance HasAbilities TheGeistTrap where
  getAbilities (TheGeistTrap attrs) =
    extendRevealed
      attrs
      [ restricted attrs 1 Here $ ActionAbility [#circle] $ ActionCost 1
      , haunted "Take 1 damage and 1 horror" attrs 2
      ]

instance RunMessage TheGeistTrap where
  runMessage msg l@(TheGeistTrap attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      circleTest sid iid (attrs.ability 1) attrs [#willpower, #intellect, #combat, #agility] (Fixed 20)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      assignDamageAndHorror iid (attrs.ability 2) 1 1
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      passedCircleTest iid attrs
      pure l
    _ -> TheGeistTrap <$> liftRunMessage msg attrs
