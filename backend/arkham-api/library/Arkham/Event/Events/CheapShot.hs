module Arkham.Event.Events.CheapShot (cheapShot) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest (getSkillTestTargetedEnemy)
import Arkham.Modifier

newtype CheapShot = CheapShot EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cheapShot :: EventCard CheapShot
cheapShot = event CheapShot Cards.cheapShot

instance RunMessage CheapShot where
  runMessage msg e@(CheapShot attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      skillTestModifier sid attrs iid (AddSkillValue #agility)
      chooseFightEnemy sid iid attrs
      pure e
    PassedThisSkillTestBy iid (isSource attrs -> True) n | n >= 2 -> do
      getSkillTestTargetedEnemy >>= traverse_ (automaticallyEvadeEnemy iid)
      pure e
    _ -> CheapShot <$> liftRunMessage msg attrs
