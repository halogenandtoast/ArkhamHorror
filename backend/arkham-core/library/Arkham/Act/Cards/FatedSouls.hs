module Arkham.Act.Cards.FatedSouls (
  FatedSouls (..),
  fatedSouls,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.CampaignLogKey
import Arkham.Classes
import Arkham.Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (EncounterDeck)
import Arkham.Placement
import Arkham.Scenarios.UnionAndDisillusion.Helpers
import Arkham.Treachery.Cards qualified as Treacheries

newtype FatedSouls = FatedSouls ActAttrs
  deriving anyclass (IsAct)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, HasAbilities)

instance HasModifiersFor FatedSouls where
  getModifiersFor (InvestigatorTarget _) (FatedSouls attrs) = do
    pure $ toModifiers attrs [CannotMove, CannotBeMoved]
  getModifiersFor _ _ = pure []

fatedSouls :: ActCard FatedSouls
fatedSouls = act (2, A) FatedSouls Cards.fatedSouls (Just $ GroupClueCost (PerPlayer 2) Anywhere)

instance RunMessage FatedSouls where
  runMessage msg a@(FatedSouls attrs) = case msg of
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      miskatonicRiver <- getJustLocationByName "Miskatonic River"
      watcher <- selectJust (OutOfPlayEnemy SetAsideZone $ enemyIs Enemies.theSpectralWatcher)
      watchersGrasp <- getSetAsideCardsMatching $ cardIs Treacheries.watchersGrasp
      watchersGaze <- getSetAsideCardsMatching $ cardIs Treacheries.watchersGaze
      locations <- selectList $ LocationIsInFrontOf Anyone
      (geistTrap, placeGeistTrap) <- placeSetAsideLocation Locations.theGeistTrap
      sidedWithTheCoven <- getHasRecord TheInvestigatorsSidedWithTheCoven
      pushAll
        $ [ PlaceEnemy watcher $ AtLocation miskatonicRiver
          , ShuffleCardsIntoDeck EncounterDeck (watchersGrasp <> watchersGaze)
          , ShuffleEncounterDiscardBackIn
          , advanceActDeck attrs
          ]
        <> map PutLocationInCenter locations
        <> [placeGeistTrap]
        <> [lightBrazier geistTrap | sidedWithTheCoven]
      pure a
    _ -> FatedSouls <$> runMessage msg attrs
