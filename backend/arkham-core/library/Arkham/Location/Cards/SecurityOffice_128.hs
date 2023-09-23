module Arkham.Location.Cards.SecurityOffice_128 (
  securityOffice_128,
  SecurityOffice_128 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (securityOffice_128)
import Arkham.Location.Helpers
import Arkham.Location.Runner

newtype SecurityOffice_128 = SecurityOffice_128 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

securityOffice_128 :: LocationCard SecurityOffice_128
securityOffice_128 =
  location SecurityOffice_128 Cards.securityOffice_128 2 (PerPlayer 1)

instance HasAbilities SecurityOffice_128 where
  getAbilities (SecurityOffice_128 x) =
    withBaseAbilities
      x
      [ limitedAbility (PlayerLimit PerTurn 1)
        $ restrictedAbility x 1 Here (ActionAbility Nothing $ ActionCost 2)
      | locationRevealed x
      ]

instance RunMessage SecurityOffice_128 where
  runMessage msg l@(SecurityOffice_128 attrs) = case msg of
    UseCardAbility iid source 1 _ _
      | isSource attrs source ->
          l
            <$ push
              ( Search
                  iid
                  source
                  (InvestigatorTarget iid)
                  [fromTopOfDeck 6]
                  mempty
                  (DrawFound iid 1)
              )
    _ -> SecurityOffice_128 <$> runMessage msg attrs
