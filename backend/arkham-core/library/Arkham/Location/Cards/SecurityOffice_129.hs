module Arkham.Location.Cards.SecurityOffice_129
  ( securityOffice_129
  , SecurityOffice_129(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards (securityOffice_129)
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Id
import Arkham.Location.Runner
import Arkham.Location.Helpers
import Arkham.Matcher
import Arkham.Message
import Arkham.Target

newtype SecurityOffice_129 = SecurityOffice_129 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
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

instance RunMessage SecurityOffice_129 where
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
