module Arkham.Types.Location.Cards.AnotherDimension
  ( anotherDimension
  , AnotherDimension(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (anotherDimension)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Location.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing
import Arkham.Types.Window

newtype AnotherDimension = AnotherDimension LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

anotherDimension :: LocationCard AnotherDimension
anotherDimension = location
  AnotherDimension
  Cards.anotherDimension
  6
  (Static 0)
  Circle
  [Square, Diamond, Triangle]

instance HasAbilities AnotherDimension where
  getAbilities (AnotherDimension attrs) =
    withBaseAbilities attrs
      $ [ mkAbility attrs 1
          $ ForcedAbility
          $ LocationLeavesPlay Timing.When
          $ LocationMatchAny
              [LocationWithEnemy AnyEnemy, LocationWithInvestigator Anyone]
        | locationRevealed attrs
        ]

instance LocationRunner env => RunMessage env AnotherDimension where
  runMessage msg l@(AnotherDimension attrs) = case msg of
    UseCardAbility _ source [Window _ (LeavePlay (LocationTarget lid))] 1 _
      | isSource attrs source -> do
        investigatorIds <- getSetList @InvestigatorId lid
        enemyIds <- selectList $ UnengagedEnemy <> EnemyAt (LocationWithId lid)
        l <$ pushAll
          ([ MoveTo (toSource attrs) iid (toId attrs) | iid <- investigatorIds ]
          <> [ EnemyMove eid lid (toId attrs) | eid <- enemyIds ]
          )
    _ -> AnotherDimension <$> runMessage msg attrs
