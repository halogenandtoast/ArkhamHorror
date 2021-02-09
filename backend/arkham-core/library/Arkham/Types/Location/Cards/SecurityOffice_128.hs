module Arkham.Types.Location.Cards.SecurityOffice_128
  ( securityOffice_128
  , SecurityOffice_128(..)
  ) where


import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype SecurityOffice_128 = SecurityOffice_128 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

securityOffice_128 :: SecurityOffice_128
securityOffice_128 = SecurityOffice_128 $ baseAttrs
  "02128"
  (Name "Security Office" Nothing)
  EncounterSet.TheMiskatonicMuseum
  2
  (PerPlayer 1)
  Diamond
  [Square]
  (singleton Miskatonic)

instance HasModifiersFor env SecurityOffice_128 where
  getModifiersFor = noModifiersFor

ability :: LocationAttrs -> Ability
ability attrs =
  (mkAbility (toSource attrs) 1 (ActionAbility Nothing $ ActionCost 2))
    { abilityLimit = PlayerLimit PerTurn 1
    }

instance ActionRunner env => HasActions env SecurityOffice_128 where
  getActions iid NonFast (SecurityOffice_128 attrs) =
    withBaseActions iid NonFast attrs
      $ pure [ ActivateCardAbilityAction iid (ability attrs) | iid `on` attrs ]
  getActions iid window (SecurityOffice_128 attrs) =
    getActions iid window attrs

instance LocationRunner env => RunMessage env SecurityOffice_128 where
  runMessage msg l@(SecurityOffice_128 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> l <$ unshiftMessage
      (SearchTopOfDeck iid (InvestigatorTarget iid) 6 mempty ShuffleBackIn)
    _ -> SecurityOffice_128 <$> runMessage msg attrs
