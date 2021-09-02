module Arkham.Types.Location.Cards.TheGuardian
  ( theGuardian
  , TheGuardian(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message
import qualified Arkham.Types.Timing as Timing

newtype TheGuardian = TheGuardian LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGuardian :: LocationCard TheGuardian
theGuardian = locationWith
  TheGuardian
  Cards.theGuardian
  3
  (PerPlayer 2)
  NoSymbol
  []
  (connectsToL .~ singleton RightOf)

instance HasAbilities TheGuardian where
  getAbilities (TheGuardian attrs) =
    withBaseAbilities attrs $
      [ mkAbility attrs 1
          $ ReactionAbility
              (Enters Timing.After You $ LocationWithId $ toId attrs)
              Free
      | locationRevealed attrs
      ]

instance LocationRunner env => RunMessage env TheGuardian where
  runMessage msg l@(TheGuardian attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (DrawCards iid 1 False)
    _ -> TheGuardian <$> runMessage msg attrs
