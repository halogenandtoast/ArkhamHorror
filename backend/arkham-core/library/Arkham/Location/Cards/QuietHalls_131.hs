module Arkham.Location.Cards.QuietHalls_131
  ( quietHalls_131
  , QuietHalls_131(..)
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

newtype QuietHalls_131 = QuietHalls_131 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quietHalls_131 :: LocationCard QuietHalls_131
quietHalls_131 = locationWith
  QuietHalls_131
  Cards.quietHalls_131
  3
  (Static 0)
  ((connectedMatchersL <>~ [LocationWithTrait SecondFloor])
  . (revealedConnectedMatchersL <>~ [LocationWithTrait SecondFloor])
  )

instance HasAbilities QuietHalls_131 where
  getAbilities (QuietHalls_131 attrs) = withBaseAbilities
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

instance RunMessage QuietHalls_131 where
  runMessage msg l@(QuietHalls_131 attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      amount <- getPlayerCount
      l <$ push (PlaceClues (toTarget attrs) amount)
    _ -> QuietHalls_131 <$> runMessage msg attrs
