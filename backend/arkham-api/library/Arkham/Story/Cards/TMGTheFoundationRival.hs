module Arkham.Story.Cards.TMGTheFoundationRival (
  tmgTheFoundationRival,
  TMGTheFoundationRival(..),
) where

import Arkham.Ability
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Message.Lifted
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype TMGTheFoundationRival = TMGTheFoundationRival StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | 'The Foundation [guardian]' rival story card (#71015b).
-- Represents conflict with The Foundation.
tmgTheFoundationRival :: StoryCard TMGTheFoundationRival
tmgTheFoundationRival = story TMGTheFoundationRival Cards.tmgTheFoundationRival

instance HasAbilities TMGTheFoundationRival where
  getAbilities (TMGTheFoundationRival attrs) =
    [ restrictedAbility attrs 1
        (exists $ enemyIs Enemies.valeriyaAntonovaDontMessWithHer <> enemyAtLocationWith You)
        parleyAction_
    , mkAbility attrs 2
        $ forced
        $ CluesOnThis (AtLeast $ StaticWithPerPlayer 1 1)
    ]

instance RunMessage TMGTheFoundationRival where
  runMessage msg s@(TMGTheFoundationRival attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      rookie <- getSetAsideCardsMatching $ cardIs Enemies.rookieCop
      pushAll [ShuffleCardsIntoDeck Deck.EncounterDeck rookie]
      valeriya <- getSetAsideCardsMatching $ cardIs Enemies.valeriyaAntonovaDontMessWithHer
      withLeadInvestigator \lid ->
        for_ valeriya \card -> withLocationOf lid (createEnemyAt_ card)
      pure s
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      enemy <- selectJust $ enemyIs Enemies.valeriyaAntonovaDontMessWithHer <> enemyAtLocationWith iid
      sid <- getRandom
      parley sid iid (attrs.ability 1) enemy #combat (Fixed 3)
      pure s
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      placeClues (attrs.ability 1) attrs 1
      pure s
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      valeriya <- selectJust $ enemyIs Enemies.valeriyaAntonovaDontMessWithHer
      pushAll
        [ AddToVictory (toTarget attrs)
        , AddToVictory (EnemyTarget valeriya)
        , RemoveAllCopiesOfEncounterCardFromGame (cardIs Enemies.rookieCop)
        ]
      removeStory attrs
      pure s
    _ -> TMGTheFoundationRival <$> liftRunMessage msg attrs
