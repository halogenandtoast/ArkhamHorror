module Arkham.Campaigns.TheScarletKeys.Key.Cards.TheLastBlossom (theLastBlossom) where

import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Cards
import Arkham.Campaigns.TheScarletKeys.Key.Import.Lifted
import Arkham.Helpers.Investigator (canHaveDamageHealed, canHaveHorrorHealed)
import Arkham.Matcher hiding (key)

newtype TheLastBlossom = TheLastBlossom ScarletKeyAttrs
  deriving anyclass (IsScarletKey, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theLastBlossom :: ScarletKeyCard TheLastBlossom
theLastBlossom = key TheLastBlossom Cards.theLastBlossom

instance RunMessage TheLastBlossom where
  runMessage msg k@(TheLastBlossom attrs) = runQueueT $ case msg of
    CampaignSpecific "shift[09544]" _ -> do
      shiftKey attrs do
        when attrs.unstable $ selectEach EnemyWithAnyDamage $ healDamageOn attrs 1
        when attrs.stable do
          withInvestigatorBearer attrs \iid -> do
            selectEach (affectsOthers $ colocatedWith iid) \iid' -> do
              whenM (canHaveDamageHealed attrs iid') $ healDamage iid attrs 1
              whenM (canHaveHorrorHealed attrs iid') $ healHorror iid attrs 1

      withInvestigatorBearer attrs (`flipOver` attrs)
      pure k
    _ -> TheLastBlossom <$> liftRunMessage msg attrs
