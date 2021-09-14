module Arkham.Types.Location.Cards.SlaughteredWoods
  ( slaughteredWoods
  , SlaughteredWoods(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (slaughteredWoods)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (RevealLocation)
import qualified Arkham.Types.Timing as Timing

newtype SlaughteredWoods = SlaughteredWoods LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

slaughteredWoods :: LocationCard SlaughteredWoods
slaughteredWoods = locationWithRevealedSideConnections
  SlaughteredWoods
  Cards.slaughteredWoods
  2
  (PerPlayer 1)
  NoSymbol
  []
  Plus
  [Triangle, Hourglass]

instance HasAbilities SlaughteredWoods where
  getAbilities (SlaughteredWoods attrs) =
    withBaseAbilities attrs
      $ [ restrictedAbility
              attrs
              1
              (InvestigatorExists $ You <> InvestigatorWithoutActionsRemaining)
            $ ForcedAbility
                (RevealLocation Timing.After You $ LocationWithId $ toId attrs)
        | locationRevealed attrs
        ]

instance LocationRunner env => RunMessage env SlaughteredWoods where
  runMessage msg l@(SlaughteredWoods attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (InvestigatorAssignDamage iid source DamageAny 0 2)
    _ -> SlaughteredWoods <$> runMessage msg attrs
