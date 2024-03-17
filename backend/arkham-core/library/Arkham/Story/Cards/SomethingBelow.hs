module Arkham.Story.Cards.SomethingBelow (SomethingBelow (..), somethingBelow) where

import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Creation
import Arkham.Helpers.Card (findJustCard)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Movement
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Target
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Zone

newtype SomethingBelow = SomethingBelow StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

somethingBelow :: StoryCard SomethingBelow
somethingBelow = story SomethingBelow Cards.somethingBelow

instance RunMessage SomethingBelow where
  runMessage msg s@(SomethingBelow attrs) = runQueueT $ case msg of
    ResolveStory iid ResolveIt story' | story' == toId attrs -> do
      seaOfBones <- selectJust $ locationIs Locations.seaOfBones
      hasDholeTunnel <-
        selectAny $ treacheryIs Treacheries.dholeTunnel <> TreacheryIsAttachedTo (toTarget seaOfBones)

      unless hasDholeTunnel $ do
        push
          $ FindEncounterCard
            iid
            (toTarget attrs)
            [FromEncounterDeck, FromEncounterDiscard]
            (cardIs Treacheries.dholeTunnel)

      mSlitheringDhole <- selectOne $ enemyIs Enemies.slitheringDhole

      case mSlitheringDhole of
        Nothing -> do
          slitheringDhole <-
            findJustCard (`cardMatch` Enemies.slitheringDhole) >>= \card ->
              createEnemyWith card seaOfBones createExhausted
          placeClues attrs slitheringDhole 2
        Just slitheringDhole -> do
          push $ Move $ move attrs slitheringDhole seaOfBones
          placeClues attrs slitheringDhole 2
      pure s
    FoundEncounterCard _ target ec | isTarget attrs target -> do
      treacheryId <- getRandom
      seaOfBones <- selectJust $ locationIs Locations.seaOfBones
      push $ AttachStoryTreacheryTo treacheryId (toCard ec) (toTarget seaOfBones)
      pure s
    _ -> SomethingBelow <$> lift (runMessage msg attrs)
