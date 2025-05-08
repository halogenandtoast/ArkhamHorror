module Arkham.Location.Cards.NotreDame (notreDame) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype NotreDame = NotreDame LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

notreDame :: LocationCard NotreDame
notreDame = location NotreDame Cards.notreDame 3 (PerPlayer 1)

instance HasModifiersFor NotreDame where
  getModifiersFor (NotreDame a) =
    whenRevealed a $ modifySelect a (enemyAt a) [EnemyFight (-1), EnemyEvade 1]

instance HasAbilities NotreDame where
  getAbilities (NotreDame a) =
    extendRevealed1 a $ skillTestAbility $ groupLimit PerGame $ restricted a 1 Here actionAbility

instance RunMessage NotreDame where
  runMessage msg l@(NotreDame attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #willpower (Fixed 6)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      mAgenda <- selectOne AgendaWithAnyDoom
      chooseOrRunOneM iid $ withI18n do
        countVar 1 $ labeled' "placeAgendaDoom" $ placeDoomOnAgenda 1
        for_ mAgenda \agenda -> do
          countVar 1 $ labeled' "removeAgendaDoom" $ removeDoom (attrs.ability 1) agenda 1
      pure l
    _ -> NotreDame <$> liftRunMessage msg attrs
