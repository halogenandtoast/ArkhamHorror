module Arkham.Event.Events.DecipheredReality5 (decipheredReality5) where

import Arkham.Discover
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted (investigate_)
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Projection

newtype DecipheredReality5 = DecipheredReality5 EventAttrs
  deriving anyclass (IsEvent, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

decipheredReality5 :: EventCard DecipheredReality5
decipheredReality5 = event DecipheredReality5 Cards.decipheredReality5

instance RunMessage DecipheredReality5 where
  runMessage msg e@(DecipheredReality5 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      locationIds <- select RevealedLocation
      maxShroud <- maximum . ncons 0 <$> traverse (fieldMap LocationShroud (fromMaybe 0)) locationIds
      sid <- getRandom
      skillTestModifier sid attrs sid (SetDifficulty maxShroud)
      investigate_ sid iid attrs
      pure e
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      -- Deciphered Reality is not a replacement effect; its effect doesn’t use
      -- any form of ‘instead’ or ‘but,’ so its effect is in addition to the
      -- standard rewards for successfully investigating.
      additionalSkillTestOptionEdit
        (optionWhenExists $ RevealedLocation <> LocationWithDiscoverableCluesBy (InvestigatorWithId iid))
        "Deciphered Reality (5)"
        (doStep 1 msg)
      pure e
    DoStep 1 (PassedThisSkillTest iid (isSource attrs -> True)) -> do
      locationIds <- select $ RevealedLocation <> LocationWithDiscoverableCluesBy (InvestigatorWithId iid)
      simultaneously $ for_ locationIds $ discoverAt IsInvestigate iid attrs 1
      pure e
    _ -> DecipheredReality5 <$> liftRunMessage msg attrs
