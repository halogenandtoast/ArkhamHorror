module Arkham.Location.Cards.NotreDame (notreDame, NotreDame (..)) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message (pattern RemoveDoom)

newtype NotreDame = NotreDame LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

notreDame :: LocationCard NotreDame
notreDame = location NotreDame Cards.notreDame 3 (PerPlayer 1)

instance HasModifiersFor NotreDame where
  getModifiersFor (NotreDame a) =
    whenRevealed a $ modifySelect a (enemyAt a) [EnemyFight (-1), EnemyEvade 1]

instance HasAbilities NotreDame where
  getAbilities (NotreDame attrs) =
    extendRevealed
      attrs
      [skillTestAbility $ groupLimit PerGame $ restrictedAbility attrs 1 Here actionAbility]

instance RunMessage NotreDame where
  runMessage msg l@(NotreDame attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #willpower (Fixed 6)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      agenda <- selectJust AnyAgenda
      hasDoom <- agendaMatches agenda AgendaWithAnyDoom
      chooseOrRunOne iid
        $ Label "Place 1 doom on current agenda" [PlaceDoom (attrs.ability 1) (toTarget agenda) 1]
        : [ Label "Remove 1 doom on current agenda" [RemoveDoom (attrs.ability 1) (toTarget agenda) 1]
          | hasDoom
          ]
      pure l
    _ -> NotreDame <$> liftRunMessage msg attrs
