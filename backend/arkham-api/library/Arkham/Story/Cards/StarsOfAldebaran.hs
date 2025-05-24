module Arkham.Story.Cards.StarsOfAldebaran (starsOfAldebaran) where

import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (DefaultReplace))
import Arkham.Message.Lifted.Choose
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype StarsOfAldebaran = StarsOfAldebaran StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

starsOfAldebaran :: StoryCard StarsOfAldebaran
starsOfAldebaran = story StarsOfAldebaran Cards.starsOfAldebaran

instance RunMessage StarsOfAldebaran where
  runMessage msg s@(StarsOfAldebaran attrs) = runQueueT $ case msg of
    ResolveStory iid _ story' | story' == toId attrs -> do
      selectEach (HealableInvestigator (toSource attrs) #horror Anyone) \iid' -> healHorror iid' attrs 3
      enemies <- select $ NotEnemy $ EnemyWithTitle "Hastur"
      chooseOrRunOneM iid $ targets enemies $ storyEnemyDamage iid 4
      bleakPlains <- selectJust $ locationIs Locations.bleakPlainsStarsOfAldebaran
      setAsideBleakPlains <- getSetAsideCardsMatching $ CardWithTitle "Bleak Plains"
      otherBleakPlain <- case setAsideBleakPlains of
        [] -> error "missing"
        (x : xs) -> sample (x :| xs)
      push $ ReplaceLocation bleakPlains otherBleakPlain DefaultReplace
      pure s
    _ -> StarsOfAldebaran <$> liftRunMessage msg attrs
