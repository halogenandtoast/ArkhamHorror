module Arkham.Enemy.Cards.RiftSeeker (riftSeeker, RiftSeeker (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype RiftSeeker = RiftSeeker EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riftSeeker :: EnemyCard RiftSeeker
riftSeeker = enemy RiftSeeker Cards.riftSeeker (3, Static 3, 4) (1, 1)

instance HasAbilities RiftSeeker where
  getAbilities (RiftSeeker a) =
    extend
      a
      [ mkAbility a 1 $ forced $ EnemyAttacks #after You AnyEnemyAttack $ (be a)
      , restricted a 2 OnSameLocation
          $ parleyAction
          $ HorrorCost (toSource a) YouTarget 2
          <> DoomCost (toSource a) (AgendaMatcherTarget AnyAgenda) 1
      ]

instance RunMessage RiftSeeker where
  runMessage msg e@(RiftSeeker attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      agendas <- selectMap AgendaTarget AnyAgenda
      chooseOneM iid do
        labeled "take 1 additional damage and 1 additional horror" do
          enemyAttackModifiers attrs attrs [DamageDealt 1, HorrorDealt 1]
        labeled "Place 1 doom on each agenda" do
          for_ agendas \target -> placeDoom (attrs.ability 1) target 1
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure e
    _ -> RiftSeeker <$> liftRunMessage msg attrs
