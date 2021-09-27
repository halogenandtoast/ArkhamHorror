module Arkham.Types.Location.Cards.RialtoBridge
  ( rialtoBridge
  , RialtoBridge(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Direction
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Timing qualified as Timing

newtype RialtoBridge = RialtoBridge LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rialtoBridge :: LocationCard RialtoBridge
rialtoBridge = locationWith
  RialtoBridge
  Cards.rialtoBridge
  2
  (Static 1)
  NoSymbol
  []
  (connectsToL .~ singleton RightOf)

instance HasAbilities RialtoBridge where
  getAbilities (RialtoBridge attrs) =
    withBaseAbilities attrs $
      [ mkAbility attrs 1
        $ ForcedAbility
        $ Leaves Timing.After You
        $ LocationWithId
        $ toId attrs
      | locationRevealed attrs
      ]

instance LocationRunner env => RunMessage env RialtoBridge where
  runMessage msg l@(RialtoBridge attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (LoseActions iid source 1)
    _ -> RialtoBridge <$> runMessage msg attrs
