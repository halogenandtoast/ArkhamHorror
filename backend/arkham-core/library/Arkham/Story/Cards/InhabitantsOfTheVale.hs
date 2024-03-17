module Arkham.Story.Cards.InhabitantsOfTheVale (InhabitantsOfTheVale (..), inhabitantsOfTheVale) where

import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Creation
import Arkham.Helpers.Message (drawCardsIfCan)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Source
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Target

newtype InhabitantsOfTheVale = InhabitantsOfTheVale StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inhabitantsOfTheVale :: StoryCard InhabitantsOfTheVale
inhabitantsOfTheVale = story InhabitantsOfTheVale Cards.inhabitantsOfTheVale

instance RunMessage InhabitantsOfTheVale where
  runMessage msg s@(InhabitantsOfTheVale attrs) = runQueueT $ case msg of
    ResolveStory iid ResolveIt story' | story' == toId attrs -> do
      valeOfPnath <- selectJust $ locationIs Locations.valeOfPnath
      findEncounterCard iid attrs (cardIs Enemies.huntingNightgaunt)

      selectEach (investigatorAt valeOfPnath) $ \iid' -> do
        mDrawing <- drawCardsIfCan iid' attrs 2
        for_ mDrawing $ \drawing -> chooseOne iid' [Label "Do no draw 2 cards" [], Label "Draw 2 cards" [drawing]]

      push $ DoStep 1 msg
      pure s
    FoundEncounterCard _ target ec | isTarget attrs target -> do
      valeOfPnath <- selectJust $ locationIs Locations.valeOfPnath
      createEnemyWith_ (toCard ec) valeOfPnath createExhausted
      pure s
    DoStep 1 (ResolveStory iid ResolveIt story') | story' == toId attrs -> do
      enemies <- select AnyEnemy
      chooseOne
        iid
        [targetLabel enemy [PlaceClues (toSource attrs) (toTarget enemy) 2] | enemy <- enemies]
      pure s
    _ -> InhabitantsOfTheVale <$> lift (runMessage msg attrs)
