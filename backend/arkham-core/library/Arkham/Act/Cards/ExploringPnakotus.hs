module Arkham.Act.Cards.ExploringPnakotus
  ( ExploringPnakotus(..)
  , exploringPnakotus
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Keyword ( Keyword (Aloof) )
import Arkham.Matcher
import Arkham.Message
import Arkham.Placement
import Arkham.Target
import Data.List.NonEmpty qualified as NE

newtype ExploringPnakotus = ExploringPnakotus ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

exploringPnakotus :: ActCard ExploringPnakotus
exploringPnakotus = act
  (1, A)
  ExploringPnakotus
  Cards.exploringPnakotus
  (Just $ GroupClueCost (PerPlayer 2) Anywhere)

instance HasModifiersFor ExploringPnakotus where
  getModifiersFor (EnemyTarget eid) (ExploringPnakotus attrs) | onSide A attrs = do
    isYithianObserver <- eid <=~> enemyIs Enemies.yithianObserver
    pure $ toModifiers attrs [ AddKeyword Aloof | isYithianObserver ]
  getModifiersFor _ _ = pure []

instance RunMessage ExploringPnakotus where
  runMessage msg a@(ExploringPnakotus attrs) = case msg of
    AdvanceAct aid _ _ | aid == actId attrs && onSide B attrs -> do
      locations <- getSetAsideCardsMatching $ CardWithType LocationType
      custodian <- getSetAsideCard Assets.theCustodian
      yithianObservers <- selectList $ enemyIs Enemies.yithianObserver

      spawnLocation <- maybe (error "no locations") sample
        $ NE.nonEmpty locations

      pushAll
        $ map EnemyCheckEngagement yithianObservers
        <> map PlaceLocation locations
        <> [ CreateAssetAt custodian $ AtLocation $ toLocationId spawnLocation
           , AdvanceActDeck (actDeckId attrs) (toSource attrs)
           ]

      pure a
    _ -> ExploringPnakotus <$> runMessage msg attrs
