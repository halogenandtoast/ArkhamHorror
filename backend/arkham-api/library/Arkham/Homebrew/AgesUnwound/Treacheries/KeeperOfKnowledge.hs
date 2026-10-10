module Arkham.Homebrew.AgesUnwound.Treacheries.KeeperOfKnowledge (keeperOfKnowledge) where

import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgesUnwound.Missions.Helpers (placeThisTask)
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype KeeperOfKnowledge = KeeperOfKnowledge TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

keeperOfKnowledge :: TreacheryCard KeeperOfKnowledge
keeperOfKnowledge = treachery KeeperOfKnowledge Cards.keeperOfKnowledge

{- | "__Revelation__ - Spawn the set-aside Ancient Sphinx enemy in Cairo. /
__Task__ - Acquire the knowledge of the sphinx."

The Ancient Sphinx prints the completion, both from /Answers/ and from its
__Forced__ "when Ancient Sphinx leaves play".
-}
instance RunMessage KeeperOfKnowledge where
  runMessage msg t@(KeeperOfKnowledge attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      placeThisTask attrs
      createSetAsideEnemy_ Enemies.ancientSphinx (locationIs Locations.cairo)
      pure t
    _ -> KeeperOfKnowledge <$> liftRunMessage msg attrs
