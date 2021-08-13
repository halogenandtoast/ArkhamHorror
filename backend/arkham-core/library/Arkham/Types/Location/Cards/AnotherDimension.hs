module Arkham.Types.Location.Cards.AnotherDimension
  ( anotherDimension
  , AnotherDimension(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (anotherDimension)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Restriction
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window (Window(..))
import qualified Arkham.Types.Window as Window

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

instance HasActions AnotherDimension where
  getActions (AnotherDimension a) =
    [ restrictedAbility
          a
          1
          (AnyRestriction
            [ InvestigatorExists $ InvestigatorAt LocationLeavingPlay
            , EnemyExists $ EnemyAt LocationLeavingPlay
            ]
          )
        $ ForcedAbility (LocationLeavesPlay Timing.When Anywhere)
    ]

instance (HasSet UnengagedEnemyId env LocationId, LocationRunner env) => RunMessage env AnotherDimension where
  runMessage msg l@(AnotherDimension attrs) = case msg of
    UseCardAbility _ source [Window _ (Window.LocationLeavesPlay lid)] 1 _
      | isSource attrs source -> do
        investigatorIds <- getSetList @InvestigatorId lid
        enemyIds <- map unUnengagedEnemyId <$> getSetList lid
        l <$ pushAll
          (concat
              [ [MoveTo iid (toId attrs), MovedBy iid (toSource attrs)]
              | iid <- investigatorIds
              ]
          <> [ EnemyMove eid lid (toId attrs) | eid <- enemyIds ]
          )
    _ -> AnotherDimension <$> runMessage msg attrs
