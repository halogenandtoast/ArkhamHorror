module Arkham.Enemy.Cards.VengefulWitch (vengefulWitch) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Enemy.Types (Field (EnemyHealthDamage, EnemySanityDamage))
import Arkham.Matcher
import Arkham.Projection

newtype VengefulWitch = VengefulWitch EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vengefulWitch :: EnemyCard VengefulWitch
vengefulWitch =
  enemy VengefulWitch Cards.vengefulWitch (3, Static 3, 3) (1, 1)
    & setSpawnAt (mapOneOf LocationWithTitle ["The Gallows", "Heretics' Graves"])

instance HasAbilities VengefulWitch where
  getAbilities (VengefulWitch a) =
    extend1 a
      $ restricted a 1 (exists $ InvestigatorAt $ locationWithEnemy (toId a))
      $ forced
      $ EnemyDefeated #when Anyone ByAny (be a)

instance RunMessage VengefulWitch where
  runMessage msg e@(VengefulWitch attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      damage <- field EnemyHealthDamage attrs.id
      horror <- field EnemySanityDamage attrs.id
      investigators <- select $ InvestigatorAt $ locationWithEnemy attrs.id
      for_ investigators \iid -> directDamageAndHorror iid (attrs.ability 1) damage horror
      pure e
    _ -> VengefulWitch <$> liftRunMessage msg attrs
