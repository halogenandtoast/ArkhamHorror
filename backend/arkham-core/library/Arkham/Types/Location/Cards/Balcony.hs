module Arkham.Types.Location.Cards.Balcony
  ( balcony
  , Balcony(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (MoveAction)
import qualified Arkham.Types.Timing as Timing

newtype Balcony = Balcony LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

balcony :: LocationCard Balcony
balcony =
  location Balcony Cards.balcony 2 (PerPlayer 1) Square [Circle, Triangle]

instance HasAbilities Balcony where
  getAbilities (Balcony attrs) = withBaseAbilities
    attrs
    [ mkAbility attrs 1 $ ForcedAbility $ MoveAction
        Timing.After
        You
        (LocationWithId $ toId attrs)
        Anywhere
    | locationRevealed attrs
    ]

instance LocationRunner env => RunMessage env Balcony where
  runMessage msg l@(Balcony attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (InvestigatorAssignDamage iid source DamageAny 2 0)
    _ -> Balcony <$> runMessage msg attrs
