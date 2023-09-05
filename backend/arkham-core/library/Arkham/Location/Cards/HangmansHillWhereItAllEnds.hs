module Arkham.Location.Cards.HangmansHillWhereItAllEnds
  ( hangmansHillWhereItAllEnds
  , HangmansHillWhereItAllEnds(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype HangmansHillWhereItAllEnds = HangmansHillWhereItAllEnds LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hangmansHillWhereItAllEnds :: LocationCard HangmansHillWhereItAllEnds
hangmansHillWhereItAllEnds = location HangmansHillWhereItAllEnds Cards.hangmansHillWhereItAllEnds 2 (Static 0)

instance HasAbilities HangmansHillWhereItAllEnds where
  getAbilities (HangmansHillWhereItAllEnds attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage HangmansHillWhereItAllEnds where
  runMessage msg (HangmansHillWhereItAllEnds attrs) =
    HangmansHillWhereItAllEnds <$> runMessage msg attrs
