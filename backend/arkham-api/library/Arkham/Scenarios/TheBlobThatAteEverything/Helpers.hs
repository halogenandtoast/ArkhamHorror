module Arkham.Scenarios.TheBlobThatAteEverything.Helpers where

import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Location (getConnectedLocations)
import Arkham.Helpers.Scenario (standaloneI18n)
import Arkham.I18n
import Arkham.Id
import Arkham.Matcher
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.Projection
import Arkham.Tracing

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = withI18n $ standaloneI18n "theBlobThatAteEverything" a

-- | Subject 8L-08, the anomaly itself. Matches both the Single Group
-- (@subject8L08@) and Epic Multiplayer (@subject8L08EpicMultiplayer@) variants;
-- only one of them is ever in play in a given game, so every existing
-- subject-targeting query keeps working in epic mode unchanged.
subject8L08Matcher :: EnemyMatcher
subject8L08Matcher = mapOneOf enemyIs [Cards.subject8L08, Cards.subject8L08EpicMultiplayer]

-- | Subject 8L-08 (either variant). The anomaly itself.
getSubject8L08 :: (HasGame m, Tracing m) => m (Maybe EnemyId)
getSubject8L08 = selectOne subject8L08Matcher

-- | The number of cards Subject 8L-08 has devoured (placed beneath it).
getDevouredCount :: (HasGame m, Tracing m) => m Int
getDevouredCount =
  getSubject8L08 >>= \case
    Nothing -> pure 0
    Just s -> length <$> field EnemyCardsUnderneath s

-- | A location cannot be devoured if removing it would cause another location
-- to have no valid connections.
canDevourLocation :: (HasGame m, Tracing m) => LocationId -> m Bool
canDevourLocation lid = do
  otherLocations <- select $ Anywhere <> not_ (LocationWithId lid)
  not <$> anyM wouldOrphan otherLocations
 where
  wouldOrphan otherLid = do
    connected <- getConnectedLocations otherLid
    pure $ lid `elem` connected && all (== lid) connected

{- | Subject 8L-08 devours the given cards: they are placed beneath it and are
considered out of play.
-}
devour :: ReverseQueue m => [Card] -> m ()
devour [] = pure ()
devour cards =
  getSubject8L08 >>= traverse_ \s -> placeUnderneath s cards
