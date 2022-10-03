module Arkham.Location.Cards.QuietHalls_135
  ( quietHalls_135
  , QuietHalls_135(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Trait

newtype QuietHalls_135 = QuietHalls_135 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quietHalls_135 :: LocationCard QuietHalls_135
quietHalls_135 = locationWith
  QuietHalls_135
  Cards.quietHalls_135
  3
  (Static 0)
  ((connectedMatchersL <>~ [LocationWithTrait ThirdFloor])
  . (revealedConnectedMatchersL <>~ [LocationWithTrait ThirdFloor])
  )

instance HasAbilities QuietHalls_135 where
  getAbilities (QuietHalls_135 attrs) = withBaseAbilities
    attrs
    [ restrictedAbility
        attrs
        1
        (Here <> Negate (LocationExists UnrevealedLocation) <> Negate
          (LocationExists LocationWithAnyClues)
        )
      $ ActionAbility Nothing
      $ ActionCost 1
    | locationRevealed attrs
    ]

instance RunMessage QuietHalls_135 where
  runMessage msg l@(QuietHalls_135 attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      amount <- getPlayerCount
      l <$ push (PlaceClues (toTarget attrs) amount)
    _ -> QuietHalls_135 <$> runMessage msg attrs
