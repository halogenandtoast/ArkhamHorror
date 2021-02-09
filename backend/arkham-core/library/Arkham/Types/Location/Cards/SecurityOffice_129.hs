module Arkham.Types.Location.Cards.SecurityOffice_129
  ( securityOffice_129
  , SecurityOffice_129(..)
  ) where


import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype SecurityOffice_129 = SecurityOffice_129 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

securityOffice_129 :: SecurityOffice_129
securityOffice_129 = SecurityOffice_129 $ baseAttrs
  "02129"
  (Name "Security Office" Nothing)
  EncounterSet.TheMiskatonicMuseum
  3
  (PerPlayer 2)
  Diamond
  [Square]
  (singleton Miskatonic)

instance HasModifiersFor env SecurityOffice_129 where
  getModifiersFor = noModifiersFor

ability :: LocationAttrs -> Ability
ability attrs =
  (mkAbility (toSource attrs) 1 (ActionAbility Nothing $ ActionCost 2))
    { abilityLimit = PlayerLimit PerTurn 1
    }

instance ActionRunner env => HasActions env SecurityOffice_129 where
  getActions iid NonFast (SecurityOffice_129 attrs) =
    withBaseActions iid NonFast attrs
      $ pure [ ActivateCardAbilityAction iid (ability attrs) | iid `on` attrs ]
  getActions iid window (SecurityOffice_129 attrs) =
    getActions iid window attrs

instance LocationRunner env => RunMessage env SecurityOffice_129 where
  runMessage msg l@(SecurityOffice_129 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      unrevealedExhibitHalls <- map unUnrevealedLocationId
        <$> getSetList (LocationWithTitle "ExhibitHall")
      l <$ unshiftMessage
        (chooseOne
          iid
          (TargetLabel
              ScenarioDeckTarget
              [LookAtTopOfDeck iid ScenarioDeckTarget 1]
          : [ LookAtRevealed exhibitHall
            | exhibitHall <- unrevealedExhibitHalls
            ]
          )
        )
    _ -> SecurityOffice_129 <$> runMessage msg attrs
