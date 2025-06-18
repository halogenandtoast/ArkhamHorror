module Arkham.Story.Cards.TheSyndicateRival (theSyndicateRival) where

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

newtype TheSyndicateRival = TheSyndicateRival StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSyndicateRival :: StoryCard TheSyndicateRival
theSyndicateRival = persistStory $ story TheSyndicateRival Cards.theSyndicateRival

instance HasAbilities TheSyndicateRival where
  getAbilities (TheSyndicateRival a) =
    [ restricted
        a
        1
        (exists $ enemyIs Enemies.johnnyValoneHereToCollect <> EnemyAt YourLocation)
        parleyAction_
    , restricted a 2 (CluesOnThis $ AtLeast $ StaticWithPerPlayer 1 1)
        $ forced AnyWindow
    ]

instance RunMessage TheSyndicateRival where
  runMessage msg s@(TheSyndicateRival attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      coldStreak <- getSetAsideCardsMatching $ cardIs Treacheries.coldStreak
      shuffleCardsIntoDeck Deck.EncounterDeck coldStreak
      johnny <- getSetAsideCardsMatching $ cardIs Enemies.johnnyValoneHereToCollect
      lead <- getLead
      for_ johnny (withLocationOf lead . createEnemyAt_)
      pure s
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemy <- selectJust $ enemyIs Enemies.johnnyValoneHereToCollect <> enemyAtLocationWith iid
      sid <- getRandom
      parley sid iid (attrs.ability 1) enemy #agility (Fixed 3)
      pure s
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      placeClues (attrs.ability 1) attrs 1
      pure s
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      johnny <- selectJust $ enemyIs Enemies.johnnyValoneHereToCollect
      addToVictory attrs
      addToVictory johnny
      push $ RemoveAllCopiesOfEncounterCardFromGame (cardIs Treacheries.coldStreak)
      removeStory attrs
      pure s
    _ -> TheSyndicateRival <$> liftRunMessage msg attrs
