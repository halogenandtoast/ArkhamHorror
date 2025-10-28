module Arkham.Act.Cards.TheForgottenIsland (theForgottenIsland) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Uses
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Scenario
import Arkham.Keyword (Keyword (Alert))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Message.Lifted.Placement
import Arkham.Scenario.Deck
import Arkham.Scenarios.FilmFatale.Helpers
import Arkham.Trait (Trait (Jungle))

newtype TheForgottenIsland = TheForgottenIsland ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theForgottenIsland :: ActCard TheForgottenIsland
theForgottenIsland = act (2, A) TheForgottenIsland Cards.theForgottenIsland Nothing

instance HasModifiersFor TheForgottenIsland where
  getModifiersFor (TheForgottenIsland a) = do
    modifySelect a (EnemyWithTitle "Possessed Extra") [HealthModifier 1, EnemyEvade 1, AddKeyword Alert]

instance HasAbilities TheForgottenIsland where
  getAbilities = actAbilities \a ->
    [ restricted
        a
        1
        ( exists (enemyIs Enemies.allosaurusIndomitablePredator)
            <> exists (locationIs Locations.ruinsOfTheSerpentKing <> LocationWithToken Seal)
        )
        $ FastAbility (SameLocationGroupClueCost (PerPlayer 1) (LocationWithTrait Jungle))
    , restricted
        a
        2
        ( EachUndefeatedInvestigator (at_ $ locationIs Locations.jungleSet)
            <> exists (assetIs Assets.staffOfTheSerpentRelicOfThePast <> at_ (locationIs Locations.jungleSet))
        )
        $ Objective
        $ forced (RoundEnds #when)
    ]

instance RunMessage TheForgottenIsland where
  runMessage msg a@(TheForgottenIsland attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      ruinsOfTheSerpentKing <- selectJust $ locationIs Locations.ruinsOfTheSerpentKing
      allosaurus <- selectJust $ enemyIs Enemies.allosaurusIndomitablePredator
      moveTokens (attrs.ability 1) ruinsOfTheSerpentKing allosaurus Seal 1
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      mallosaurus <- selectOne $ InPlayEnemy $ enemyIs Enemies.allosaurusRampagingPredator
      for_ mallosaurus (`place` OutOfPlay SetAsideZone)
      discardEach attrs (not_ $ enemyIs Enemies.allosaurusRampagingPredator)
      jungleSet <- selectJust $ locationIs Locations.jungleSet
      eachInvestigator \iid -> do
        moveTo attrs iid jungleSet
        discardAllClues attrs iid

      selectEach (LocationWithTrait Jungle) removeLocation

      placeSetAsideLocations_ [Locations.spaceSet, Locations.gothicSet]
      push $ SetLayout initialLayout
      lead <- getLead
      centralLot <- placeSetAsideLocation Locations.centralLotQuietOnSet
      flipOverBy lead attrs centralLot

      for_ mallosaurus \allosaurus -> do
        push $ EnemySpawnFromOutOfPlay SetAsideZone Nothing centralLot allosaurus
        flipOverBy lead attrs allosaurus

      reelDeck <- take 2 <$> getScenarioDeck ReelDeck
      shuffleCardsIntoDeck Deck.EncounterDeck reelDeck
      shuffleEncounterDiscardBackIn

      addChaosToken #cultist
      addChaosToken #tablet
      addChaosToken #elderthing

      advanceActDeck attrs
      pure a
    _ -> TheForgottenIsland <$> liftRunMessage msg attrs
