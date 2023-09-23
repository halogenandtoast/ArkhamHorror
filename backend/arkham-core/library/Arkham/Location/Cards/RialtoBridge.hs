module Arkham.Location.Cards.RialtoBridge (
  rialtoBridge,
  RialtoBridge (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype RialtoBridge = RialtoBridge LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rialtoBridge :: LocationCard RialtoBridge
rialtoBridge =
  locationWith
    RialtoBridge
    Cards.rialtoBridge
    2
    (Static 1)
    (connectsToL .~ singleton RightOf)

instance HasAbilities RialtoBridge where
  getAbilities (RialtoBridge attrs) =
    withBaseAbilities attrs
      $ [ mkAbility attrs 1
          $ ForcedAbility
          $ Leaves Timing.After You
          $ LocationWithId
          $ toId attrs
        | locationRevealed attrs
        ]

instance RunMessage RialtoBridge where
  runMessage msg l@(RialtoBridge attrs) = case msg of
    UseCardAbility iid source 1 _ _
      | isSource attrs source ->
          l <$ push (LoseActions iid source 1)
    _ -> RialtoBridge <$> runMessage msg attrs
