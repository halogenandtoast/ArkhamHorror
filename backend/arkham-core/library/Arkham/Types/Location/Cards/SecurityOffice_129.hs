module Arkham.Types.Location.Cards.SecurityOffice_129
  ( securityOffice_129
  , SecurityOffice_129(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (securityOffice_129)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target

newtype SecurityOffice_129 = SecurityOffice_129 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

securityOffice_129 :: LocationCard SecurityOffice_129
securityOffice_129 = location
  SecurityOffice_129
  Cards.securityOffice_129
  3
  (PerPlayer 2)
  Diamond
  [Square]

instance HasAbilities SecurityOffice_129 where
  getAbilities (SecurityOffice_129 x) = withBaseAbilities
    x
    [ restrictedAbility x 1 Here (ActionAbility Nothing $ ActionCost 2)
        & (abilityLimitL .~ PlayerLimit PerTurn 1)
    | locationRevealed x
    ]

instance LocationRunner env => RunMessage env SecurityOffice_129 where
  runMessage msg l@(SecurityOffice_129 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      unrevealedExhibitHalls <- map unUnrevealedLocationId
        <$> getSetList (LocationWithTitle "ExhibitHall")
      l <$ push
        (chooseOne
          iid
          (TargetLabel
              ScenarioDeckTarget
              [LookAtTopOfDeck iid ScenarioDeckTarget 1]
          : [ LookAtRevealed (toSource attrs) (LocationTarget exhibitHall)
            | exhibitHall <- unrevealedExhibitHalls
            ]
          )
        )
    _ -> SecurityOffice_129 <$> runMessage msg attrs
