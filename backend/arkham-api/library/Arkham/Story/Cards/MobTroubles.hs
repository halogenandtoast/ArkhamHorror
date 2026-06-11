module Arkham.Story.Cards.MobTroubles (mobTroubles) where

import Arkham.Ability
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query (getLead, getSetAsideCardsMatching)
import Arkham.Helpers.Scenario (scenarioField)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Types (Field (..))
import Arkham.ScenarioLogKey
import Arkham.Scenarios.MachinationsThroughTime.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Strategy

newtype MobTroubles = MobTroubles StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mobTroubles :: StoryCard MobTroubles
mobTroubles = story MobTroubles Cards.mobTroubles & persistStory

instance HasAbilities MobTroubles where
  getAbilities (MobTroubles a) =
    if a ^. flippedL
      then
        [ restricted a 1 (Remembered TheDebtHasBeenPaid)
            $ actionAbilityWithCost (GroupClueCost (PerPlayer 1) Anywhere)
        ]
      else []

instance RunMessage MobTroubles where
  runMessage msg s@(MobTroubles attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      inPlay <- select $ enemyIs Enemies.sheldonGang
      inDiscard <-
        filter (`cardMatch` cardIs Enemies.sheldonGang)
          . map toCard
          <$> scenarioField ScenarioDiscard
      chooseOneM iid $ scenarioI18n do
        for_ inPlay \gang -> targeting gang do
          addToVictory iid gang
          doStep 1 msg
        when (notNull inDiscard) do
          labeled' "mobTroubles.searchDiscard" do
            for_ (take 1 inDiscard) \card -> do
              obtainCard card
              push $ AddToVictory (Just iid) (CardIdTarget $ toCardId card)
            doStep 1 msg
        labeled' "mobTroubles.searchDeck" do
          search iid (attrs.ability 1) EncounterDeckTarget [fromDeck] (basic $ cardIs Enemies.sheldonGang)
            $ defer attrs IsNotDraw
      pure s
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      for_ (take 1 cards) \card -> do
        obtainCard card
        push $ AddToVictory (Just iid) (CardIdTarget $ toCardId card)
      doStep 1 msg
      pure s
    DoStep 1 _ -> do
      n <- selectCount $ VictoryDisplayCardMatch $ basic $ cardIs Enemies.sheldonGang
      when (n >= 3) do
        lead <- getLead
        addToVictory lead attrs
      pure s
    Flip _ _ (isTarget attrs -> True) -> do
      gangs <- getSetAsideCardsMatching (cardIs Enemies.sheldonGang)
      shuffleCardsIntoDeck Deck.EncounterDeck gangs
      tickTock <- selectOne $ locationIs Locations.tickTockClubPresent
      for_ tickTock \club -> do
        sadie <- getSetAsideCardsMatching (cardIs Enemies.oldSadieSheldon)
        for_ sadie \card -> createEnemyAt_ card club
      pure $ MobTroubles $ attrs & flippedL .~ True
    _ -> MobTroubles <$> liftRunMessage msg attrs
