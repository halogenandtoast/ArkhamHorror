module Arkham.Treachery.Cards.OppressiveInfluence (oppressiveInfluence) where

import Arkham.Helpers.GameValue (getGameValue)
import Arkham.Helpers.Investigator (getMaybeLocation)
import Arkham.Location.Types (Field (LocationPrintedShroud))
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype OppressiveInfluence = OppressiveInfluence TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oppressiveInfluence :: TreacheryCard OppressiveInfluence
oppressiveInfluence = treachery OppressiveInfluence Cards.oppressiveInfluence

instance RunMessage OppressiveInfluence where
  runMessage msg t@(OppressiveInfluence attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      -- X is the location's *printed* shroud, so modifiers to its shroud do not
      -- move the difficulty.
      difficulty <- runMaybeT do
        lid <- MaybeT $ getMaybeLocation iid
        value <- MaybeT $ field LocationPrintedShroud lid
        lift $ getGameValue value
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed $ fromMaybe 0 difficulty)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignHorror iid attrs 2
      pure t
    _ -> OppressiveInfluence <$> liftRunMessage msg attrs
