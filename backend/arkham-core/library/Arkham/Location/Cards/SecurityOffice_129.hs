module Arkham.Location.Cards.SecurityOffice_129 (
  securityOffice_129,
  SecurityOffice_129 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (securityOffice_129)
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher

newtype SecurityOffice_129 = SecurityOffice_129 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

securityOffice_129 :: LocationCard SecurityOffice_129
securityOffice_129 =
  location SecurityOffice_129 Cards.securityOffice_129 3 (PerPlayer 2)

instance HasAbilities SecurityOffice_129 where
  getAbilities (SecurityOffice_129 x) =
    withBaseAbilities
      x
      [ limitedAbility (PlayerLimit PerTurn 1) $
        restrictedAbility x 1 Here (ActionAbility Nothing $ ActionCost 2)
      | locationRevealed x
      ]

instance RunMessage SecurityOffice_129 where
  runMessage msg l@(SecurityOffice_129 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      unrevealedExhibitHalls <-
        selectList $ UnrevealedLocation <> LocationWithTitle "ExhibitHall"
      push $
        chooseOne iid $
          TargetLabel
            ScenarioDeckTarget
            [LookAtTopOfDeck iid ScenarioDeckTarget 1]
            : [ targetLabel
                exhibitHall
                [LookAtRevealed iid (toSource attrs) (LocationTarget exhibitHall)]
              | exhibitHall <- unrevealedExhibitHalls
              ]
      pure l
    _ -> SecurityOffice_129 <$> runMessage msg attrs
