module Arkham.Story.Cards.TMGLocalsOfKingsportRival (
  tmgLocalsOfKingsportRival,
  TMGLocalsOfKingsportRival(..),
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

newtype TMGLocalsOfKingsportRival = TMGLocalsOfKingsportRival StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tmgLocalsOfKingsportRival :: StoryCard TMGLocalsOfKingsportRival
tmgLocalsOfKingsportRival = story TMGLocalsOfKingsportRival Cards.tmgLocalsOfKingsportRival

instance HasAbilities TMGLocalsOfKingsportRival where
  getAbilities (TMGLocalsOfKingsportRival attrs) =
    [ restrictedAbility attrs 1
        (exists $ enemyIs Enemies.williamBainUntrustworthy <> enemyAtLocationWith You)
        parleyAction_
    , mkAbility attrs 2
        $ forced
        $ CluesOnThis (AtLeast $ StaticWithPerPlayer 1 1)
    ]

instance RunMessage TMGLocalsOfKingsportRival where
  runMessage msg s@(TMGLocalsOfKingsportRival attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      unlucky <- getSetAsideCardsMatching $ cardIs Treacheries.unlucky
      for_ unlucky \card -> push $ ShuffleCardsIntoDeck Deck.EncounterDeck [card]
      bain <- getSetAsideCardsMatching $ cardIs Enemies.williamBainUntrustworthy
      withLeadInvestigator \lid ->
        for_ bain \card -> withLocationOf lid (createEnemyAt_ card)
      pure s
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      enemy <- selectJust $ enemyIs Enemies.williamBainUntrustworthy <> enemyAtLocationWith iid
      sid <- getRandom
      parley sid iid (attrs.ability 1) enemy #any (Fixed 4)
      pure s
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      placeClues (attrs.ability 1) attrs 1
      pure s
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      bain <- selectJust $ enemyIs Enemies.williamBainUntrustworthy
      pushAll
        [ AddToVictory (toTarget attrs)
        , AddToVictory (EnemyTarget bain)
        , RemoveAllCopiesOfEncounterCardFromGame (cardIs Treacheries.unlucky)
        ]
      removeStory attrs
      pure s
    _ -> TMGLocalsOfKingsportRival <$> liftRunMessage msg attrs

