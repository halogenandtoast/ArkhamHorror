module Arkham.Location.Cards.HereticsGraves_171 (
  hereticsGraves_171,
  HereticsGraves_171 (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Runner
import Arkham.SkillType

newtype HereticsGraves_171 = HereticsGraves_171 LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, HasAbilities)

hereticsGraves_171 :: LocationCard HereticsGraves_171
hereticsGraves_171 = location HereticsGraves_171 Cards.hereticsGraves_171 7 (Static 0)

instance HasModifiersFor HereticsGraves_171 where
  getModifiersFor (InvestigatorTarget iid) (HereticsGraves_171 a) = do
    investigating <- isInvestigating iid (toId a)
    willpower <- getSkillValue SkillWillpower iid
    pure $ toModifiers a [AnySkillValue willpower | investigating]
  getModifiersFor _ _ = pure []

instance RunMessage HereticsGraves_171 where
  runMessage msg l@(HereticsGraves_171 attrs) = case msg of
    Flip _ _ target | isTarget attrs target -> do
      spectral <- genCard Locations.hereticsGravesSpectral_171
      push $ ReplaceLocation (toId attrs) spectral Swap
      pure l
    _ -> HereticsGraves_171 <$> runMessage msg attrs
