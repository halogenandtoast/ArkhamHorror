module Arkham.Types.Location.Cards.ScienceBuilding where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (scienceBuilding)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target

newtype ScienceBuilding = ScienceBuilding LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scienceBuilding :: LocationId -> ScienceBuilding
scienceBuilding = ScienceBuilding . baseAttrs
  Cards.scienceBuilding
  2
  (PerPlayer 1)
  Hourglass
  [Plus, Squiggle]

instance HasModifiersFor env ScienceBuilding where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env ScienceBuilding where
  getActions i window (ScienceBuilding attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env ScienceBuilding where
  runMessage msg l@(ScienceBuilding attrs) = case msg of
    RevealLocation _ lid | lid == locationId attrs -> do
      unshiftMessage $ PlaceLocationMatching (LocationWithTitle "Alchemy Labs")
      ScienceBuilding <$> runMessage msg attrs
    FailedSkillTest iid _ (SkillTestSource _ SkillWillpower _ _ _) SkillTestInitiatorTarget{} _ _
      | iid `elem` locationInvestigators attrs
      -> l <$ unshiftMessage
        (InvestigatorAssignDamage iid (toSource attrs) DamageAny 1 0)
    _ -> ScienceBuilding <$> runMessage msg attrs
