module Arkham.Types.Location.Cards.QuietHalls_131
  ( quietHalls_131
  , QuietHalls_131(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Trait

newtype QuietHalls_131 = QuietHalls_131 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quietHalls_131 :: LocationCard QuietHalls_131
quietHalls_131 = locationWith
  QuietHalls_131
  Cards.quietHalls_131
  3
  (Static 0)
  Circle
  [Square, Star]
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

instance LocationRunner env => RunMessage env QuietHalls_131 where
  runMessage msg l@(QuietHalls_131 attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      amount <- getPlayerCount
      l <$ push (PlaceClues (toTarget attrs) amount)
    _ -> QuietHalls_131 <$> runMessage msg attrs
