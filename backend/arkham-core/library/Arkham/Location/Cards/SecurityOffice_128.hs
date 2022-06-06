module Arkham.Location.Cards.SecurityOffice_128
  ( securityOffice_128
  , SecurityOffice_128(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards (securityOffice_128)
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Runner
import Arkham.Location.Helpers
import Arkham.Message
import Arkham.Target

newtype SecurityOffice_128 = SecurityOffice_128 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

securityOffice_128 :: LocationCard SecurityOffice_128
securityOffice_128 = location
  SecurityOffice_128
  Cards.securityOffice_128
  2
  (PerPlayer 1)
  Diamond
  [Square]

instance HasAbilities SecurityOffice_128 where
  getAbilities (SecurityOffice_128 x) = withBaseAbilities
    x
    [ restrictedAbility x 1 Here (ActionAbility Nothing $ ActionCost 2)
        & (abilityLimitL .~ PlayerLimit PerTurn 1)
    | locationRevealed x
    ]

instance LocationRunner env => RunMessage SecurityOffice_128 where
  runMessage msg l@(SecurityOffice_128 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> l <$ push
      (Search
        iid
        source
        (InvestigatorTarget iid)
        [fromTopOfDeck 6]
        mempty
        (DrawFound iid 1)
      )
    _ -> SecurityOffice_128 <$> runMessage msg attrs
