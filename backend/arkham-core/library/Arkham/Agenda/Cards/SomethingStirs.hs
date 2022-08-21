module Arkham.Agenda.Cards.SomethingStirs
  ( SomethingStirs(..)
  , somethingStirs
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype SomethingStirs = SomethingStirs AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

somethingStirs :: AgendaCard SomethingStirs
somethingStirs =
  agenda (1, A) SomethingStirs Cards.somethingStirs (StaticWithPerPlayer 6 1)

instance HasAbilities SomethingStirs where
  getAbilities (SomethingStirs a) =
    [ mkAbility a 1
        $ ForcedAbility
        $ AgendaAdvances Timing.When
        $ AgendaWithId
        $ toId a
    ]

-- ability does not do anything, just triggers the button
instance RunMessage SomethingStirs where
  runMessage msg a@(SomethingStirs attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      _locations <- select Anywhere
      -- pushAll $ map
      --   (CreateWindowModifierEffect
      --     EffectPhaseWindow
      --     (EffectModifiers $ toModifier attrs DoNotRemoveDoom)
      --     source
      --   )
      --   locations
      pure a
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
      a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
    _ -> SomethingStirs <$> runMessage msg attrs
