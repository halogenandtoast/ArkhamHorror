module Arkham.Agenda.Cards.DeathsApproach (
  DeathsApproach (..),
  deathsApproach,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Trait (Trait (Spectral))

-- Normally text like "Locations cannot be flipped" would be handled by
-- HasModifiers, however we are letting the specific cards (Agenda, Fast
-- Abilities, etc.) handle that themselves. If another Scenario does something
-- similar we should encode as a ModifierType

newtype DeathsApproach = DeathsApproach AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- Errata: The text on this card should read: "Locations cannot be flipped to their non-spectral side"
--
instance HasModifiersFor DeathsApproach where
  getModifiersFor (LocationTarget lid) (DeathsApproach attrs) = do
    isSpectral <- lid <=~> LocationWithTrait Spectral
    pure $ toModifiers attrs [CannotBeFlipped | isSpectral]
  getModifiersFor _ _ = pure []

deathsApproach :: AgendaCard DeathsApproach
deathsApproach = agenda (2, A) DeathsApproach Cards.deathsApproach (Static 7)

instance RunMessage DeathsApproach where
  runMessage msg a@(DeathsApproach attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
        push $ scenarioResolution 2
        pure a
      _ -> DeathsApproach <$> runMessage msg attrs
