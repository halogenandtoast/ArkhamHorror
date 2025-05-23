module Arkham.Enemy.Cards.RiftSeeker (riftSeeker) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.BlackStarsRise.Helpers

newtype RiftSeeker = RiftSeeker EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riftSeeker :: EnemyCard RiftSeeker
riftSeeker = enemy RiftSeeker Cards.riftSeeker (3, Static 3, 4) (1, 1)

instance HasAbilities RiftSeeker where
  getAbilities (RiftSeeker a) =
    extend
      a
      [ mkAbility a 1 $ forced $ EnemyAttacks #after You AnyEnemyAttack (be a)
      , restricted a 2 OnSameLocation
          $ parleyAction
          $ HorrorCost (toSource a) YouTarget 2
          <> DoomCost (toSource a) (AgendaMatcherTarget AnyAgenda) 1
      ]

instance RunMessage RiftSeeker where
  runMessage msg e@(RiftSeeker attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> scenarioI18n do
      agendas <- selectMap AgendaTarget AnyAgenda
      chooseOneM iid do
        labeled' "riftSeeker.takeDamageAndHorror" do
          assignDamageAndHorror iid (attrs.ability 1) 1 1
        labeled' "riftSeeker.doom" do
          for_ agendas (placeDoomOn (attrs.ability 1) 1)
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure e
    _ -> RiftSeeker <$> liftRunMessage msg attrs
