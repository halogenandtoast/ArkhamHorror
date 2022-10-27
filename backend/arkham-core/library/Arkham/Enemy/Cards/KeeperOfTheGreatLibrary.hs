module Arkham.Enemy.Cards.KeeperOfTheGreatLibrary
  ( keeperOfTheGreatLibrary
  , KeeperOfTheGreatLibrary(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Classes
import Arkham.Enemy.Runner

newtype KeeperOfTheGreatLibrary = KeeperOfTheGreatLibrary EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

keeperOfTheGreatLibrary :: EnemyCard KeeperOfTheGreatLibrary
keeperOfTheGreatLibrary = enemy KeeperOfTheGreatLibrary Cards.keeperOfTheGreatLibrary (3, Static 4, 3) (1, 1)

instance RunMessage KeeperOfTheGreatLibrary where
  runMessage msg (KeeperOfTheGreatLibrary attrs) =
    KeeperOfTheGreatLibrary <$> runMessage msg attrs
