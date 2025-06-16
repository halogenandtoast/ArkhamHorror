module Arkham.Story.Cards.TMGSilverTwilightLodgeRival (
  tmgSilverTwilightLodgeRival,
  TMGSilverTwilightLodgeRival(..),
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

newtype TMGSilverTwilightLodgeRival = TMGSilverTwilightLodgeRival StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tmgSilverTwilightLodgeRival :: StoryCard TMGSilverTwilightLodgeRival
tmgSilverTwilightLodgeRival = story TMGSilverTwilightLodgeRival Cards.tmgSilverTwilightLodgeRival

instance HasAbilities TMGSilverTwilightLodgeRival where
  getAbilities (TMGSilverTwilightLodgeRival attrs) =
    [ restrictedAbility attrs 1
        (exists $ enemyIs Enemies.carlSanfordTheLodgeIsMine <> enemyAtLocationWith You)
        parleyAction_
    , mkAbility attrs 2
        $ forced
        $ CluesOnThis (AtLeast $ StaticWithPerPlayer 1 1)
    ]

instance RunMessage TMGSilverTwilightLodgeRival where
  runMessage msg s@(TMGSilverTwilightLodgeRival attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      ward <- getSetAsideCardsMatching $ cardIs Treacheries.wardOfPreservation
      for_ ward \card -> push $ ShuffleCardsIntoDeck Deck.EncounterDeck [card]
      carl <- getSetAsideCardsMatching $ cardIs Enemies.carlSanfordTheLodgeIsMine
      withLeadInvestigator \lid ->
        for_ carl \card -> withLocationOf lid (createEnemyAt_ card)
      pure s
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      enemy <- selectJust $ enemyIs Enemies.carlSanfordTheLodgeIsMine <> enemyAtLocationWith iid
      sid <- getRandom
      parley sid iid (attrs.ability 1) enemy #willpower (Fixed 3)
      pure s
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      placeClues (attrs.ability 1) attrs 1
      pure s
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      carl <- selectJust $ enemyIs Enemies.carlSanfordTheLodgeIsMine
      pushAll
        [ AddToVictory (toTarget attrs)
        , AddToVictory (EnemyTarget carl)
        , RemoveAllCopiesOfEncounterCardFromGame (cardIs Treacheries.wardOfPreservation)
        ]
      removeStory attrs
      pure s
    _ -> TMGSilverTwilightLodgeRival <$> liftRunMessage msg attrs

