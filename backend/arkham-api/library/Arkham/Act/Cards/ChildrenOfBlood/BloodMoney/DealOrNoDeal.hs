module Arkham.Act.Cards.ChildrenOfBlood.BloodMoney.DealOrNoDeal (dealOrNoDeal) where

import Arkham.Ability
import Arkham.Act.CardDefs.ChildrenOfBlood.BloodMoney qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.ChildrenOfBlood.Helpers
import Arkham.Deck qualified as Deck
import Arkham.Enemy.CardDefs.ChildrenOfBlood.BloodMoney qualified as Enemies
import Arkham.Helpers.GameValue (getGameValue)
import Arkham.Helpers.Query (getPlayerCount, getSetAsideCardsMatching)
import Arkham.Location.CardDefs.ChildrenOfBlood.BloodMoney qualified as Locations
import Arkham.Enemy.Types (Field (EnemySealedChaosTokens))
import Arkham.Location.Types (Field (LocationClues, LocationRevealClues))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Trait (Trait (Cultist))
import Arkham.Treachery.CardDefs.ChildrenOfBlood.BloodMoney qualified as Treacheries

newtype DealOrNoDeal = DealOrNoDeal ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dealOrNoDeal :: ActCard DealOrNoDeal
dealOrNoDeal = act (1, A) DealOrNoDeal Cards.dealOrNoDeal Nothing

instance HasAbilities DealOrNoDeal where
  getAbilities = actAbilities \x ->
    [ restricted
        x
        1
        (exists $ ReadyEnemy <> enemyIs Enemies.suspiciousGuest <> at_ YourLocation)
        $ FastAbility (OrCost [ClueCost (Static 1), ResourceCost 2])
    , restricted x 2 (notExists $ enemyIs Enemies.priscillaThomas)
        $ Objective
        $ forced
        $ RoundEnds #when
    ]

instance RunMessage DealOrNoDeal where
  runMessage msg a@(DealOrNoDeal attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      guests <- select $ ReadyEnemy <> enemyIs Enemies.suspiciousGuest <> at_ (locationWithInvestigator iid)
      chooseTargetM iid guests $ codexOn iid (attrs.ability 1) 1
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      placeSetAsideLocation_ Locations.masterBedroom
      whenM (selectAny $ SetAsideCardMatch $ cardIs Locations.balcony)
        $ placeSetAsideLocation_ Locations.balcony

      perPlayer <- getPlayerCount
      selectEach (RevealedLocation <> not_ LocationWithVictory) \lid -> do
        threshold <- getGameValue =<< field LocationRevealClues lid
        current <- field LocationClues lid
        placeClues (toSource attrs) lid (min perPlayer (max 0 (threshold - current)))

      cultists <- getSetAsideCardsMatching (#enemy <> CardWithTrait Cultist)
      rebirths <- getSetAsideCardsMatching (cardIs Treacheries.sanguineRebirth)
      shuffleCardsIntoDeck Deck.EncounterDeck (cultists <> rebirths)

      selectEach (EnemyWithSealedChaosTokens 1 AnyChaosToken) \eid ->
        traverse_ unsealChaosToken =<< field EnemySealedChaosTokens eid

      advanceActDeck attrs
      pure a
    _ -> DealOrNoDeal <$> liftRunMessage msg attrs
