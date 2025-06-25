module Arkham.Story.Cards.SilverTwilightLodgeRival (silverTwilightLodgeRival) where

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

newtype SilverTwilightLodgeRival = SilverTwilightLodgeRival StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

silverTwilightLodgeRival :: StoryCard SilverTwilightLodgeRival
silverTwilightLodgeRival = persistStory $ story SilverTwilightLodgeRival Cards.silverTwilightLodgeRival

instance HasAbilities SilverTwilightLodgeRival where
  getAbilities (SilverTwilightLodgeRival attrs) =
    [ restricted
        attrs
        1
        (exists $ enemyIs Enemies.carlSanfordDeathlessFanatic <> EnemyAt YourLocation)
        parleyAction_
    , restricted attrs 2 (CluesOnThis $ AtLeast $ StaticWithPerPlayer 1 1)
        $ forced AnyWindow
    ]

instance RunMessage SilverTwilightLodgeRival where
  runMessage msg s@(SilverTwilightLodgeRival attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      ward <- getSetAsideCardsMatching $ cardIs Treacheries.wardOfPreservation
      shuffleCardsIntoDeck Deck.EncounterDeck ward
      carl <- getSetAsideCardsMatching $ cardIs Enemies.carlSanfordDeathlessFanatic
      lead <- getLead
      for_ carl $ withLocationOf lead . createEnemyAt_
      pure s
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemy <- selectJust $ enemyIs Enemies.carlSanfordDeathlessFanatic <> enemyAtLocationWith iid
      sid <- getRandom
      parley sid iid (attrs.ability 1) enemy #willpower (Fixed 3)
      pure s
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      whenMatch iid InvestigatorWithAnyClues do
        moveTokens (attrs.ability 1) iid attrs #clue 1
      pure s
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      carl <- selectJust $ enemyIs Enemies.carlSanfordDeathlessFanatic
      addToVictory attrs
      addToVictory carl
      push $ RemoveAllCopiesOfEncounterCardFromGame (cardIs Treacheries.wardOfPreservation)
      removeStory attrs
      pure s
    _ -> SilverTwilightLodgeRival <$> liftRunMessage msg attrs
