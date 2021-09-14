module Arkham.Types.Location.Cards.QuietHalls_135
  ( quietHalls_135
  , QuietHalls_135(..)
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

newtype QuietHalls_135 = QuietHalls_135 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quietHalls_135 :: LocationCard QuietHalls_135
quietHalls_135 = locationWith
  QuietHalls_135
  Cards.quietHalls_135
  3
  (Static 0)
  Star
  [Circle]
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

instance LocationRunner env => RunMessage env QuietHalls_135 where
  runMessage msg l@(QuietHalls_135 attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      amount <- getPlayerCount
      l <$ push (PlaceClues (toTarget attrs) amount)
    _ -> QuietHalls_135 <$> runMessage msg attrs
