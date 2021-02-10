module Arkham.Types.Location.Cards.ExhibitHallRestrictedHall
  ( exhibitHallRestrictedHall
  , ExhibitHallRestrictedHall(..)
  ) where

import Arkham.Prelude

import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.LocationSymbol
import Arkham.Types.Modifier
import Arkham.Types.Name
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype ExhibitHallRestrictedHall = ExhibitHallRestrictedHall LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exhibitHallRestrictedHall :: ExhibitHallRestrictedHall
exhibitHallRestrictedHall = ExhibitHallRestrictedHall
  $ base { locationVictory = Just 1 }
 where
  base = baseAttrs
    "02137"
    (Name "Exhibit Hall" $ Just "Restricted Hall")
    EncounterSet.TheMiskatonicMuseum
    3
    (PerPlayer 2)
    Equals
    [Square]
    [Miskatonic, Exhibit]

instance HasId (Maybe StoryEnemyId) env CardCode => HasModifiersFor env ExhibitHallRestrictedHall where
  getModifiersFor _ target (ExhibitHallRestrictedHall attrs)
    | isTarget attrs target = do
      mHuntingHorror <- fmap unStoryEnemyId <$> getId (CardCode "02141")
      case mHuntingHorror of
        Just eid | eid `member` locationEnemies attrs ->
          pure $ toModifiers attrs [CannotInvestigate]
        _ -> pure []
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env ExhibitHallRestrictedHall where
  getActions iid window (ExhibitHallRestrictedHall attrs) =
    getActions iid window attrs

instance LocationRunner env => RunMessage env ExhibitHallRestrictedHall where
  runMessage msg (ExhibitHallRestrictedHall attrs) =
    ExhibitHallRestrictedHall <$> runMessage msg attrs
