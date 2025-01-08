module Arkham.Enemy.Cards.PrimordialEvil (primordialEvil) where

import Arkham.Ability
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Capability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype PrimordialEvil = PrimordialEvil EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

primordialEvil :: EnemyCard PrimordialEvil
primordialEvil = enemy PrimordialEvil Cards.primordialEvil (3, Static 5, 1) (2, 1)

instance HasAbilities PrimordialEvil where
  getAbilities (PrimordialEvil a) =
    extend
      a
      [mkAbility a 1 $ forced $ EnemyDealtDamage #when AnyDamageEffect (be a) (SourceOwnedBy You)]

instance RunMessage PrimordialEvil where
  runMessage msg e@(PrimordialEvil attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      canManipulateDeck <- can.manipulate.deck iid
      tekelili <- getTekelili 1

      if not canManipulateDeck || null tekelili
        then initiateEnemyAttack attrs (attrs.ability 1) iid
        else addTekelili iid tekelili
      pure e
    _ -> PrimordialEvil <$> liftRunMessage msg attrs
