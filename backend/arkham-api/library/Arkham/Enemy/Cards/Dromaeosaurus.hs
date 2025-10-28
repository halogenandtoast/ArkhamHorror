module Arkham.Enemy.Cards.Dromaeosaurus (dromaeosaurus) where

import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype Dromaeosaurus = Dromaeosaurus EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dromaeosaurus :: EnemyCard Dromaeosaurus
dromaeosaurus = enemy Dromaeosaurus Cards.dromaeosaurus (3, Static 2, 4) (1, 0)

instance HasAbilities Dromaeosaurus where
  getAbilities (Dromaeosaurus a) = extend1 a $ mkAbility a 1 $ forced $ EnemyEngaged #after You (be a)

instance RunMessage Dromaeosaurus where
  runMessage msg e@(Dromaeosaurus attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      hasDiscards <- selectAny $ inHandOf NotForPlay iid <> basic DiscardableCard
      chooseOneM iid $ withI18n do
        countVar 1 $ labeledValidate' hasDiscards "discardCards" $ chooseAndDiscardCard iid (attrs.ability 1)
        nameVar attrs $ labeled' "attacksYou" $ initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> Dromaeosaurus <$> liftRunMessage msg attrs
