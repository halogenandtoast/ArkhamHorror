module Arkham.Story.Cards.ReturnToThePathToCarcosa.ReturnToDimCarcosa.HastursLastStand (hastursLastStand) where

import Arkham.Enemy.CardDefs.ThePathToCarcosa.DimCarcosa qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.Location.CardDefs.ReturnToThePathToCarcosa.ReturnToDimCarcosa qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Projection
import Arkham.Story.CardDefs.ReturnToThePathToCarcosa.ReturnToDimCarcosa qualified as Cards
import Arkham.Story.Import.Lifted

newtype HastursLastStand = HastursLastStand StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hastursLastStand :: StoryCard HastursLastStand
hastursLastStand = story HastursLastStand Cards.hastursLastStand

instance RunMessage HastursLastStand where
  runMessage msg s@(HastursLastStand attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      hastur <- selectJust (EnemyWithTitle "Hastur")
      code <- field EnemyCardCode hastur
      if
        | code == Enemies.hasturTheKingInYellow.cardCode ->
            placeSetAsideLocation_ Locations.recessesOfYourOwnMind
        | code == Enemies.hasturLordOfCarcosa.cardCode -> do
            theThroneRoom <- placeSetAsideLocation Locations.theThroneRoom
            enemyMoveTo attrs hastur theThroneRoom
            readyThis hastur
        | otherwise -> placeSetAsideLocation_ Locations.stageOfTheWardTheatre

      pure s
    _ -> HastursLastStand <$> liftRunMessage msg attrs
