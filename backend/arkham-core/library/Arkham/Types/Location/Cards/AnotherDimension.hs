module Arkham.Types.Location.Cards.AnotherDimension
  ( anotherDimension
  , AnotherDimension(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (anotherDimension)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Location.Attrs
import Arkham.Types.Message
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
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

forcedAbility :: LocationAttrs -> LocationId -> Ability
forcedAbility a lid =
  mkAbility (toSource a) 1 LegacyForcedAbility
    & abilityMetadataL
    ?~ TargetMetadata (LocationTarget lid)

instance ActionRunner env => HasAbilities env AnotherDimension where
  getAbilities iid (Window Timing.When (LeavePlay (LocationTarget lid))) (AnotherDimension attrs)
    = do
      leadInvestigator <- getLeadInvestigatorId
      investigatorIds <- getSet @InvestigatorId lid
      pure
        [ forcedAbility attrs lid
        | iid == leadInvestigator && notNull investigatorIds
        ]
  getAbilities iid window (AnotherDimension attrs) =
    getAbilities iid window attrs

instance (HasSet UnengagedEnemyId env LocationId, LocationRunner env) => RunMessage env AnotherDimension where
  runMessage msg l@(AnotherDimension attrs) = case msg of
    UseCardAbility _ source [Window _ (LeavePlay (LocationTarget lid))] 1 _
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
