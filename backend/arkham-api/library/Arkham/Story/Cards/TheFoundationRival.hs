module Arkham.Story.Cards.TheFoundationRival (theFoundationRival) where

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

newtype TheFoundationRival = TheFoundationRival StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | 'The Foundation [guardian]' rival story card (#71015b).
Represents conflict with The Foundation.
-}
theFoundationRival :: StoryCard TheFoundationRival
theFoundationRival = persistStory $ story TheFoundationRival Cards.theFoundationRival

instance HasAbilities TheFoundationRival where
  getAbilities (TheFoundationRival a) =
    [ restricted
        a
        1
        (exists $ enemyIs Enemies.valeriyaAntonovaDontMessWithHer <> EnemyAt YourLocation)
        parleyAction_
    , restricted a 2 (CluesOnThis $ AtLeast $ StaticWithPerPlayer 1 1)
        $ forced AnyWindow
    ]

instance RunMessage TheFoundationRival where
  runMessage msg s@(TheFoundationRival attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      rookie <- getSetAsideCardsMatching $ cardIs Enemies.rookieCop
      shuffleCardsIntoDeck Deck.EncounterDeck rookie
      valeriya <- getSetAsideCardsMatching $ cardIs Enemies.valeriyaAntonovaDontMessWithHer
      lead <- getLead
      for_ valeriya $ withLocationOf lead . createEnemyAt_
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
      addToVictory attrs
      addToVictory valeriya
      push $ RemoveAllCopiesOfEncounterCardFromGame (cardIs Enemies.rookieCop)
      removeStory attrs
      pure s
    _ -> TheFoundationRival <$> liftRunMessage msg attrs
