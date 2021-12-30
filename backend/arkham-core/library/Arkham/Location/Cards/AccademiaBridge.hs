module Arkham.Location.Cards.AccademiaBridge
  ( accademiaBridge
  , AccademiaBridge(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Classes
import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Runner
import Arkham.Location.Helpers
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype AccademiaBridge = AccademiaBridge LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

accademiaBridge :: LocationCard AccademiaBridge
accademiaBridge = locationWith
  AccademiaBridge
  Cards.accademiaBridge
  2
  (PerPlayer 1)
  NoSymbol
  []
  (connectsToL .~ singleton RightOf)

instance HasAbilities AccademiaBridge where
  getAbilities (AccademiaBridge attrs) =
    withBaseAbilities attrs $
      [ mkAbility attrs 1
        $ ForcedAbility
        $ Leaves Timing.After You
        $ LocationWithId
        $ toId attrs
      | locationRevealed attrs
      ]

instance LocationRunner env => RunMessage env AccademiaBridge where
  runMessage msg l@(AccademiaBridge attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (LoseResources iid 2)
    _ -> AccademiaBridge <$> runMessage msg attrs
