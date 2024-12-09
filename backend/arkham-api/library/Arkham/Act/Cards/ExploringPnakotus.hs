module Arkham.Act.Cards.ExploringPnakotus (
  ExploringPnakotus (..),
  exploringPnakotus,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Keyword (Keyword (Aloof))
import Arkham.Matcher
import Arkham.Placement
import Data.List.NonEmpty qualified as NE

newtype ExploringPnakotus = ExploringPnakotus ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

exploringPnakotus :: ActCard ExploringPnakotus
exploringPnakotus =
  act
    (1, A)
    ExploringPnakotus
    Cards.exploringPnakotus
    (Just $ GroupClueCost (PerPlayer 2) Anywhere)

instance HasModifiersFor ExploringPnakotus where
  getModifiersFor (ExploringPnakotus attrs) =
    if onSide A attrs
      then
        modifySelectWith attrs (enemyIs Enemies.yithianObserver) setActiveDuringSetup [AddKeyword Aloof]
      else pure mempty

instance RunMessage ExploringPnakotus where
  runMessage msg a@(ExploringPnakotus attrs) = case msg of
    AdvanceAct aid _ _ | aid == actId attrs && onSide B attrs -> do
      locations <- getSetAsideCardsMatching LocationCard
      custodian <- getSetAsideCard Assets.theCustodian
      yithianObservers <- select $ enemyIs Enemies.yithianObserver
      placements <- traverse placeLocation locations
      spawnLocation <- maybe (error "no locations") (fmap fst . sample) $ NE.nonEmpty placements
      assetId <- getRandom

      pushAll
        $ map EnemyCheckEngagement yithianObservers
        <> map snd placements
        <> [ CreateAssetAt assetId custodian $ AtLocation spawnLocation
           , AdvanceActDeck (actDeckId attrs) (toSource attrs)
           ]

      pure a
    _ -> ExploringPnakotus <$> runMessage msg attrs
