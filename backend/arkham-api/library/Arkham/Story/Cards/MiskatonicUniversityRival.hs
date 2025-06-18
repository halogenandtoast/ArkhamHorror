module Arkham.Story.Cards.MiskatonicUniversityRival (miskatonicUniversityRival) where

import Arkham.Ability
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Helpers.Location
import Arkham.Helpers.Query
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Matcher
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Treachery.Cards qualified as Treacheries

newtype MiskatonicUniversityRival = MiskatonicUniversityRival StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miskatonicUniversityRival :: StoryCard MiskatonicUniversityRival
miskatonicUniversityRival = persistStory $ story MiskatonicUniversityRival Cards.miskatonicUniversityRival

instance HasAbilities MiskatonicUniversityRival where
  getAbilities (MiskatonicUniversityRival a) =
    [ restricted
        a
        1
        (exists $ enemyIs Enemies.caldwellPhilipsCompelledByDreams <> EnemyAt YourLocation)
        parleyAction_
    , restricted a 2 (CluesOnThis $ AtLeast $ StaticWithPerPlayer 1 1)
        $ forced AnyWindow
    ]

instance RunMessage MiskatonicUniversityRival where
  runMessage msg s@(MiskatonicUniversityRival attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      confusion <- getSetAsideCardsMatching $ cardIs Treacheries.confusion
      shuffleCardsIntoDeck Deck.EncounterDeck confusion
      caldwell <- getSetAsideCardsMatching $ cardIs Enemies.caldwellPhilipsCompelledByDreams
      lead <- getLead
      for_ caldwell $ withLocationOf lead . createEnemyAt_
      pure s
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemy <- selectJust $ enemyIs Enemies.caldwellPhilipsCompelledByDreams <> enemyAtLocationWith iid
      sid <- getRandom
      parley sid iid (attrs.ability 1) enemy #intellect (Fixed 3)
      pure s
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      placeClues (attrs.ability 1) attrs 1
      pure s
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      addToVictory attrs
      caldwell <- selectJust $ enemyIs Enemies.caldwellPhilipsCompelledByDreams
      addToVictory caldwell
      push $ RemoveAllCopiesOfEncounterCardFromGame (cardIs Treacheries.confusion)
      removeStory attrs
      pure s
    _ -> MiskatonicUniversityRival <$> liftRunMessage msg attrs
