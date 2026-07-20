module Arkham.Story.Cards.MobTroubles (mobTroubles) where

import Arkham.Ability
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.ScenarioLogKey
import Arkham.Scenarios.MachinationsThroughTime.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype MobTroubles = MobTroubles StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mobTroubles :: StoryCard MobTroubles
mobTroubles = story MobTroubles Cards.mobTroubles & persistStory

instance HasAbilities MobTroubles where
  getAbilities (MobTroubles a) =
    guard a.flipped
      *> [ noAOO
             $ restricted a 1 (Remembered TheDebtHasBeenPaid)
             $ actionAbilityWithCost (GroupClueCost (PerPlayer 1) Anywhere)
         , onlyOnce
             $ restricted a 2 (InVictoryDisplay (cardIs Enemies.sheldonGang) (static 3))
             $ Objective
             $ forced AnyWindow
         ]

instance RunMessage MobTroubles where
  runMessage msg s@(MobTroubles attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      inPlay <- select $ enemyIs Enemies.sheldonGang
      chooseOrRunOneM iid $ scenarioI18n do
        targets inPlay $ addToVictory iid
        labeled' "mobTroubles.search" do
          findEncounterCardIn iid attrs (cardIs Enemies.sheldonGang) [#deck, #discard]
      pure s
    FoundEncounterCardFrom iid (isTarget attrs -> True) _ card -> do
      addToVictory iid (CardIdTarget $ toCardId card)
      pure s
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      addToVictory iid attrs
      pure s
    Flip _ _ (isTarget attrs -> True) -> do
      gangs <- getSetAsideCardsMatching (cardIs Enemies.sheldonGang)
      shuffleCardsIntoDeck Deck.EncounterDeck gangs
      withMatch (locationIs Locations.tickTockClubPresent) \club -> do
        sadie <- getSetAsideCardsMatching (cardIs Enemies.oldSadieSheldon)
        for_ sadie (`createEnemyAt_` club)
      pure $ MobTroubles $ attrs & flippedL .~ True
    _ -> MobTroubles <$> liftRunMessage msg attrs
