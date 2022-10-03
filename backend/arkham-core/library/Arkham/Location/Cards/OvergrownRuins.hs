module Arkham.Location.Cards.OvergrownRuins
  ( overgrownRuins
  , OvergrownRuins(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Treacheries

newtype OvergrownRuins = OvergrownRuins LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

overgrownRuins :: LocationCard OvergrownRuins
overgrownRuins = location OvergrownRuins Cards.overgrownRuins 5 (PerPlayer 1)

instance HasAbilities OvergrownRuins where
  getAbilities (OvergrownRuins a) =
    withBaseAbilities a
      $ [ restrictedAbility
            a
            1
            (TreacheryExists
            $ treacheryIs Treacheries.poisoned
            <> TreacheryInThreatAreaOf You
            )
          $ ForcedAbility
          $ Enters Timing.After You
          $ LocationWithId
          $ toId a
        ]

instance RunMessage OvergrownRuins where
  runMessage msg l@(OvergrownRuins attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      pushAll [SetActions iid source 0, ChooseEndTurn iid]
      pure l
    _ -> OvergrownRuins <$> runMessage msg attrs
