module Arkham.Story.Cards.LocalsOfKingsportRival (localsOfKingsportRival) where

import Arkham.Ability
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Helpers.Location
import Arkham.Helpers.Query
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Treachery.Cards qualified as Treacheries

newtype LocalsOfKingsportRival = LocalsOfKingsportRival StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

localsOfKingsportRival :: StoryCard LocalsOfKingsportRival
localsOfKingsportRival = persistStory $ story LocalsOfKingsportRival Cards.localsOfKingsportRival

instance HasAbilities LocalsOfKingsportRival where
  getAbilities (LocalsOfKingsportRival a) =
    [ restricted
        a
        1
        (exists $ enemyIs Enemies.williamBainDefiantToTheLast <> EnemyAt YourLocation)
        parleyAction_
    , restricted a 2 (CluesOnThis $ AtLeast $ StaticWithPerPlayer 1 1) $ forced AnyWindow
    ]

instance RunMessage LocalsOfKingsportRival where
  runMessage msg s@(LocalsOfKingsportRival attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      unlucky <- getSetAsideCardsMatching $ cardIs Treacheries.unlucky
      shuffleCardsIntoDeck Deck.EncounterDeck unlucky
      bain <- getSetAsideCardsMatching $ cardIs Enemies.williamBainDefiantToTheLast
      for_ bain \card -> do
        lead <- getLead
        withLocationOf lead (createEnemyAt_ card)
      pure s
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      enemy <- selectJust $ enemyIs Enemies.williamBainDefiantToTheLast <> enemyAtLocationWith iid
      sid <- getRandom
      chooseOneM iid do
        for_ [minBound ..] \kind ->
          skillLabeled kind $ parley sid iid (attrs.ability 1) enemy kind (Fixed 4)
      pure s
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      placeClues (attrs.ability 1) attrs 1
      pure s
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      bain <- selectJust $ enemyIs Enemies.williamBainDefiantToTheLast
      addToVictory attrs
      addToVictory bain
      push $ RemoveAllCopiesOfEncounterCardFromGame (cardIs Treacheries.unlucky)
      removeStory attrs
      pure s
    _ -> LocalsOfKingsportRival <$> liftRunMessage msg attrs
