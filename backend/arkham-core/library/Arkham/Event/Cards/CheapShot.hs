module Arkham.Event.Cards.CheapShot (cheapShot, CheapShot (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Fight
import Arkham.Helpers.SkillTest (getSkillTestTarget)
import Arkham.Modifier

newtype CheapShot = CheapShot EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cheapShot :: EventCard CheapShot
cheapShot = event CheapShot Cards.cheapShot

instance RunMessage CheapShot where
  runMessage msg e@(CheapShot attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      skillTestModifier attrs iid (AddSkillValue #agility)
      pushM $ mkChooseFight iid attrs
      pure e
    PassedThisSkillTestBy iid (isSource attrs -> True) n | n >= 2 -> do
      getSkillTestTarget >>= \case
        Just (EnemyTarget eid) -> push $ EnemyEvaded iid eid
        _ -> pure ()
      pure e
    _ -> CheapShot <$> lift (runMessage msg attrs)
