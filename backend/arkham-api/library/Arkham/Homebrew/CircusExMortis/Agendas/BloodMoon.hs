module Arkham.Homebrew.CircusExMortis.Agendas.BloodMoon (bloodMoon) where

import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Modifiers (modifySelectMap)
import Arkham.Homebrew.CircusExMortis.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (
  adjacentMoonlitForestConnection,
  moonlitForests,
  sufferTraumaAndDefeat,
 )

newtype BloodMoon = BloodMoon AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloodMoon :: AgendaCard BloodMoon
bloodMoon =
  agenda (2, A) BloodMoon Cards.bloodMoon (Static 9)

instance HasModifiersFor BloodMoon where
  -- "Adjacent copies of Moonlit Forest are connected to each other."
  getModifiersFor (BloodMoon a) =
    modifySelectMap a moonlitForests \lid -> [adjacentMoonlitForestConnection lid]

instance RunMessage BloodMoon where
  runMessage msg a@(BloodMoon attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      sufferTraumaAndDefeat attrs
      pure a
    _ -> BloodMoon <$> liftRunMessage msg attrs
