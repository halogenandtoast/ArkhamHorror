module Arkham.Act.Cards.CrumblingRuin (crumblingRuin) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.GameValue (getGameValue, perPlayer)
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Trait (Trait (Ancient, Elite))
import Arkham.Treachery.Cards qualified as Treacheries

newtype CrumblingRuin = CrumblingRuin ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crumblingRuin :: ActCard CrumblingRuin
crumblingRuin = act (1, A) CrumblingRuin Cards.crumblingRuin Nothing

instance HasAbilities CrumblingRuin where
  getAbilities (CrumblingRuin a) =
    extend
      a
      [ restricted a 1 (OnLocation LocationWithoutClues) exploreAction_
      , restricted a 2 (EachUndefeatedInvestigator (at_ $ locationIs Locations.innerChamber))
          $ Objective
          $ FastAbility
          $ GroupClueCost (PerPlayer 4) (locationIs Locations.innerChamber)
      ]

instance RunMessage CrumblingRuin where
  runMessage msg a@(CrumblingRuin attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      runExplore iid (attrs.ability 1)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithClues attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      innerChamber <- selectJust $ locationIs Locations.innerChamber
      selectEach (UnengagedEnemy <> EnemyWithTrait Elite) \enemy ->
        enemyMoveTo attrs enemy innerChamber
      relics <- getSetAsideCardsMatching (CardWithType EncounterAssetType <> CardWithTrait Ancient)
      vengeantPast <- getSetAsideCardsMatching (cardIs Treacheries.vengeantPast)
      vengeance <- getVengeanceInVictoryDisplay
      broods <- take vengeance <$> getSetAsideCardsMatching (cardIs Enemies.broodOfYig)
      shuffleCardsIntoDeck ExplorationDeck $ relics <> vengeantPast <> broods
      selectEach SingleSidedLocation $ shuffleIntoDeck (Deck.ScenarioDeckByKey ExplorationDeck)
      doStep 1 msg
      advanceActDeck attrs
      pure a
    DoStep 1 (AdvanceAct (isSide B attrs -> True) _ _) -> do
      n <- perPlayer 1
      selectEach RevealedLocation \lid -> do
        current <- field LocationClues lid
        threshold <- getGameValue =<< field LocationRevealClues lid
        let amount = min n (max 0 (threshold - current))
        when (amount > 0) $ placeClues attrs lid amount
      pure a
    _ -> CrumblingRuin <$> liftRunMessage msg attrs
