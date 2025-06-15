module Arkham.Enemy.Cards.CaldwellPhilipsCompelledByDreams (
  caldwellPhilipsCompelledByDreams,
  CaldwellPhilipsCompelledByDreams(..),
) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCard)
import Arkham.Matcher
import Arkham.Trait

newtype CaldwellPhilipsCompelledByDreams = CaldwellPhilipsCompelledByDreams EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

caldwellPhilipsCompelledByDreams :: EnemyCard CaldwellPhilipsCompelledByDreams
caldwellPhilipsCompelledByDreams =
  enemy CaldwellPhilipsCompelledByDreams Cards.caldwellPhilipsCompelledByDreams (2, Static 2, 3) (0, 1)

instance HasModifiersFor CaldwellPhilipsCompelledByDreams where
  getModifiersFor (CaldwellPhilipsCompelledByDreams a) = modifySelf a [CannotBeDamaged]

instance HasAbilities CaldwellPhilipsCompelledByDreams where
  getAbilities (CaldwellPhilipsCompelledByDreams a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ TurnEnds #when (InvestigatorAt $ locationWithEnemy a)

instance RunMessage CaldwellPhilipsCompelledByDreams where
  runMessage msg e@(CaldwellPhilipsCompelledByDreams attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ toMessage $ (chooseAndDiscardCard iid (attrs.ability 1)) { discardFilter = NonWeakness }
      pure e
    _ -> CaldwellPhilipsCompelledByDreams <$> liftRunMessage msg attrs
