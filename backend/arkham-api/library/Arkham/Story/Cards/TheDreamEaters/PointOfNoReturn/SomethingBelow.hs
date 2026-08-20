module Arkham.Story.Cards.TheDreamEaters.PointOfNoReturn.SomethingBelow (somethingBelow) where

import Arkham.Card
import Arkham.Enemy.CardDefs.TheDreamEaters.PointOfNoReturn qualified as Enemies
import Arkham.Enemy.Creation
import Arkham.Helpers.Card (findJustCard)
import Arkham.Location.CardDefs.TheDreamEaters.PointOfNoReturn qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Story.CardDefs.TheDreamEaters.PointOfNoReturn qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Treachery.CardDefs.TheDreamEaters.PointOfNoReturn qualified as Treacheries

newtype SomethingBelow = SomethingBelow StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

somethingBelow :: StoryCard SomethingBelow
somethingBelow = story SomethingBelow Cards.somethingBelow

instance RunMessage SomethingBelow where
  runMessage msg s@(SomethingBelow attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      seaOfBones <- selectJust $ locationIs Locations.seaOfBones
      hasDholeTunnel <-
        selectAny $ treacheryIs Treacheries.dholeTunnel <> TreacheryIsAttachedTo (toTarget seaOfBones)

      unless hasDholeTunnel do
        findEncounterCard iid attrs (cardIs Treacheries.dholeTunnel)

      selectOne (enemyIs Enemies.slitheringDhole) >>= \case
        Nothing -> do
          slitheringDhole <-
            findJustCard (`cardMatch` Enemies.slitheringDhole) >>= \card ->
              createEnemyWith card seaOfBones createExhausted
          placeClues attrs slitheringDhole 2
        Just slitheringDhole -> do
          enemyMoveTo attrs slitheringDhole seaOfBones
          placeClues attrs slitheringDhole 2
      pure s
    FoundEncounterCard _ target ec | isTarget attrs target -> do
      treacheryId <- getRandom
      seaOfBones <- selectJust $ locationIs Locations.seaOfBones
      push $ AttachStoryTreacheryTo treacheryId (toCard ec) (toTarget seaOfBones)
      pure s
    _ -> SomethingBelow <$> liftRunMessage msg attrs
