module Arkham.Types.Location.Cards.AnotherDimension
  ( anotherDimension
  , AnotherDimension(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (anotherDimension)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.InvestigatorId
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Window

newtype AnotherDimension = AnotherDimension LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

anotherDimension :: LocationId -> AnotherDimension
anotherDimension = AnotherDimension . baseAttrs
  Cards.anotherDimension
  6
  (Static 0)
  Circle
  [Square, Diamond, Triangle]

instance HasModifiersFor env AnotherDimension where
  getModifiersFor = noModifiersFor

forcedAbility :: LocationAttrs -> LocationId -> Ability
forcedAbility a lid =
  mkAbility (toSource a) 1 ForcedAbility & abilityMetadataL ?~ TargetMetadata
    (LocationTarget lid)

instance ActionRunner env => HasActions env AnotherDimension where
  getActions iid (WhenLocationLeavesPlay lid) (AnotherDimension attrs) = do
    leadInvestigator <- getLeadInvestigatorId
    investigatorIds <- getSet @InvestigatorId lid
    pure
      [ ActivateCardAbilityAction iid (forcedAbility attrs lid)
      | iid == leadInvestigator && notNull investigatorIds
      ]
  getActions iid window (AnotherDimension attrs) = getActions iid window attrs

instance (HasSet UnengagedEnemyId env LocationId, LocationRunner env) => RunMessage env AnotherDimension where
  runMessage msg l@(AnotherDimension attrs) = case msg of
    UseCardAbility _ source (Just (TargetMetadata (LocationTarget lid))) 1 _
      | isSource attrs source -> do
        investigatorIds <- getSetList @InvestigatorId lid
        enemyIds <- map unUnengagedEnemyId <$> getSetList lid
        l <$ unshiftMessages
          ([ MoveTo iid (toId attrs) | iid <- investigatorIds ]
          ++ [ EnemyMove eid lid (toId attrs) | eid <- enemyIds ]
          )
    _ -> AnotherDimension <$> runMessage msg attrs
