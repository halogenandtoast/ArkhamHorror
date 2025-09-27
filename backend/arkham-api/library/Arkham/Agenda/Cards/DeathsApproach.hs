module Arkham.Agenda.Cards.DeathsApproach (deathsApproach) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Trait (Trait (Spectral))

newtype DeathsApproach = DeathsApproach AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- Errata: The text on this card should read: "Locations cannot be flipped to their non-spectral side"
instance HasModifiersFor DeathsApproach where
  getModifiersFor (DeathsApproach attrs) = do
    modifySelect attrs (LocationWithTrait Spectral) [CannotBeFlipped]

deathsApproach :: AgendaCard DeathsApproach
deathsApproach = agenda (2, A) DeathsApproach Cards.deathsApproach (Static 7)

instance RunMessage DeathsApproach where
  runMessage msg a@(DeathsApproach attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      push R2
      pure a
    _ -> DeathsApproach <$> liftRunMessage msg attrs
