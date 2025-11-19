module Arkham.Enemy.Cards.PrimordialEvil (primordialEvil) where

import Arkham.Ability
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Shuffle (getCanShuffleIn)
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
      tekelili <- getTekelili 1
      getCanShuffleIn iid tekelili >>= \case
        True -> addTekelili iid tekelili
        False -> initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> PrimordialEvil <$> liftRunMessage msg attrs
