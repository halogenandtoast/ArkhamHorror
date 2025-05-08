module Arkham.Enemy.Cards.MadPatient (madPatient) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype MadPatient = MadPatient EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

madPatient :: EnemyCard MadPatient
madPatient =
  enemy MadPatient Cards.madPatient (2, Static 2, 3) (1, 0)
    & setPrey MostRemainingSanity
    & setSpawnAt (NearestLocationToYou "Asylum Halls")

instance HasAbilities MadPatient where
  getAbilities (MadPatient a) =
    extend a [mkAbility a 1 $ forced $ EnemyAttacked #when You AnySource (be a)]

instance RunMessage MadPatient where
  runMessage msg e@(MadPatient attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      pure e
    _ -> MadPatient <$> liftRunMessage msg attrs
