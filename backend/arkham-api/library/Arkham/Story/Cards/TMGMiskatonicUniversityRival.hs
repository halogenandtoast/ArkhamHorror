module Arkham.Story.Cards.TMGMiskatonicUniversityRival (
  tmgMiskatonicUniversityRival,
  TMGMiskatonicUniversityRival(..),
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

newtype TMGMiskatonicUniversityRival = TMGMiskatonicUniversityRival StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tmgMiskatonicUniversityRival :: StoryCard TMGMiskatonicUniversityRival
tmgMiskatonicUniversityRival = story TMGMiskatonicUniversityRival Cards.tmgMiskatonicUniversityRival

instance HasAbilities TMGMiskatonicUniversityRival where
  getAbilities (TMGMiskatonicUniversityRival attrs) =
    [ restrictedAbility attrs 1
        (exists $ enemyIs Enemies.caldwellPhilipsCompelledByDreams <> enemyAtLocationWith You)
        parleyAction_
    , mkAbility attrs 2
        $ forced
        $ CluesOnThis (AtLeast $ StaticWithPerPlayer 1 1)
    ]

instance RunMessage TMGMiskatonicUniversityRival where
  runMessage msg s@(TMGMiskatonicUniversityRival attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      confusion <- getSetAsideCardsMatching $ cardIs Treacheries.confusion
      for_ confusion \card -> push $ ShuffleCardsIntoDeck Deck.EncounterDeck [card]
      caldwell <- getSetAsideCardsMatching $ cardIs Enemies.caldwellPhilipsCompelledByDreams
      withLeadInvestigator \lid ->
        for_ caldwell \card -> withLocationOf lid (createEnemyAt_ card)
      pure s
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      enemy <- selectJust $ enemyIs Enemies.caldwellPhilipsCompelledByDreams <> enemyAtLocationWith iid
      sid <- getRandom
      parley sid iid (attrs.ability 1) enemy #intellect (Fixed 3)
      pure s
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      placeClues (attrs.ability 1) attrs 1
      pure s
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      caldwell <- selectJust $ enemyIs Enemies.caldwellPhilipsCompelledByDreams
      pushAll
        [ AddToVictory (toTarget attrs)
        , AddToVictory (EnemyTarget caldwell)
        , RemoveAllCopiesOfEncounterCardFromGame (cardIs Treacheries.confusion)
        ]
      removeStory attrs
      pure s
    _ -> TMGMiskatonicUniversityRival <$> liftRunMessage msg attrs
