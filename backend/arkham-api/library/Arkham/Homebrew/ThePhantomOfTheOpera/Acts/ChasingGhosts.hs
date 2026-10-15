module Arkham.Homebrew.ThePhantomOfTheOpera.Acts.ChasingGhosts (chasingGhosts) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.Helpers (whenExposed)
import Arkham.Direction
import Arkham.Helpers.Location (placedLocationDirection)
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Helpers.Window (getAttackDetails)
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.ThePhantomOfTheOpera.Import
import Arkham.Homebrew.ThePhantomOfTheOpera.Traits
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Placement

newtype ChasingGhosts = ChasingGhosts ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chasingGhosts :: ActCard ChasingGhosts
chasingGhosts = act (2, A) ChasingGhosts Cards.chasingGhosts Nothing

parisSewers :: LocationMatcher
parisSewers = LocationWithTitle "Paris Sewers"

instance HasAbilities ChasingGhosts where
  getAbilities = actAbilities \a ->
    [ restricted a 1 (exists $ enemyIs Enemies.thePhantom)
        $ forced
        $ CampaignEvent #after (Just You) "exposed[decoy]"
    , scenarioI18n
        $ withI18nTooltip "chasingGhosts.misdirect"
        $ mkAbility a 2
        $ triggered
          (EnemyWouldAttack #when Anyone (CancelableEnemyAttack AnyEnemyAttack) (enemyIs Enemies.thePhantom))
          (clueCost 1)
    , mkAbility a 3 $ forced $ whenExposed Enemies.thePhantom
    ]

instance RunMessage ChasingGhosts where
  runMessage msg a@(ChasingGhosts attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectForMaybeM (enemyIs Enemies.thePhantom) \phantom ->
        initiateEnemyAttack phantom (attrs.ability 1) iid
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 (getAttackDetails -> details) _ -> do
      cancelAttack (attrs.ability 2) details
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      placeTokens (attrs.ability 3) attrs #clue 1
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      sewers <- shuffle =<< getSetAsideCardsMatching (#location <> CardWithTitle "Paris Sewers")
      let (chosen, removed) = splitAt 3 sewers
      placed <- placeRandomLocationGroupCapture "parisSewers" chosen
      for_ (zip placed (drop 1 placed)) \(left, right) -> placedLocationDirection right RightOf left
      for_ removed removeCardFromGame
      placeSetAsideLocation_ Locations.thePhantomsLair
      push $ SetLayout ["parisSewers1 parisSewers2 parisSewers3", ". plus ."]
      topUpConcealedDecoys
      doStep 1 msg
      advanceActDeck attrs
      pure a
    DoStep 1 (AdvanceAct (isSide B attrs -> True) _ _) -> do
      redistributeConcealed parisSewers
      leftmost <- selectJust $ LocationWithLabel "parisSewers1"
      moveAllTo attrs leftmost
      selectEach (EnemyAt Anywhere <> not_ (EnemyWithPlacement InTheShadows)) \eid ->
        enemyMoveTo attrs eid leftmost
      selectEach (LocationWithTrait OperaHouse <> RevealedLocation) removeLocation
      selectEach (LocationWithTrait OperaHouse <> UnrevealedLocation) removeLocationWithoutVictory
      pure a
    _ -> ChasingGhosts <$> liftRunMessage msg attrs
