module Arkham.Story.Cards.TMGTheSyndicateRival (
  tmgTheSyndicateRival,
  TMGTheSyndicateRival(..),
) where

import Arkham.Ability
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Message.Lifted
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Treachery.Cards qualified as Treacheries

newtype TMGTheSyndicateRival = TMGTheSyndicateRival StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tmgTheSyndicateRival :: StoryCard TMGTheSyndicateRival
tmgTheSyndicateRival = story TMGTheSyndicateRival Cards.tmgTheSyndicateRival

instance HasAbilities TMGTheSyndicateRival where
  getAbilities (TMGTheSyndicateRival attrs) =
    [ restrictedAbility attrs 1
        (exists $ enemyIs Enemies.johnnyValoneHereToCollect <> enemyAtLocationWith You)
        parleyAction_
    , mkAbility attrs 2
        $ forced
        $ CluesOnThis (AtLeast $ StaticWithPerPlayer 1 1)
    ]

instance RunMessage TMGTheSyndicateRival where
  runMessage msg s@(TMGTheSyndicateRival attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      coldStreak <- getSetAsideCardsMatching $ cardIs Treacheries.coldStreak
      for_ coldStreak \card -> push $ ShuffleCardsIntoDeck Deck.EncounterDeck [card]
      johnny <- getSetAsideCardsMatching $ cardIs Enemies.johnnyValoneHereToCollect
      withLeadInvestigator \lid ->
        for_ johnny \card -> withLocationOf lid (createEnemyAt_ card)
      pure s
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      enemy <- selectJust $ enemyIs Enemies.johnnyValoneHereToCollect <> enemyAtLocationWith iid
      sid <- getRandom
      parley sid iid (attrs.ability 1) enemy #agility (Fixed 3)
      pure s
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      placeClues (attrs.ability 1) attrs 1
      pure s
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      johnny <- selectJust $ enemyIs Enemies.johnnyValoneHereToCollect
      pushAll
        [ AddToVictory (toTarget attrs)
        , AddToVictory (EnemyTarget johnny)
        , RemoveAllCopiesOfEncounterCardFromGame (cardIs Treacheries.coldStreak)
        ]
      removeStory attrs
      pure s
    _ -> TMGTheSyndicateRival <$> liftRunMessage msg attrs
