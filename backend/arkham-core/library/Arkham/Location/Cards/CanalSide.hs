module Arkham.Location.Cards.CanalSide
  ( canalSide
  , CanalSide(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype CanalSide = CanalSide LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

canalSide :: LocationCard CanalSide
canalSide = locationWith
  CanalSide
  Cards.canalSide
  2
  (Static 1)
  (connectsToL .~ singleton RightOf)

instance HasAbilities CanalSide where
  getAbilities (CanalSide attrs) =
    withBaseAbilities attrs
      $ [ mkAbility attrs 1
            $ ReactionAbility
                (Enters Timing.After You $ LocationWithId $ toId attrs)
                Free
        | locationRevealed attrs
        ]

instance RunMessage CanalSide where
  runMessage msg l@(CanalSide attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source ->
      l <$ push (PlaceClues (toTarget attrs) 1)
    _ -> CanalSide <$> runMessage msg attrs
