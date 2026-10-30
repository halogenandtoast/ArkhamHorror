module Arkham.Homebrew.NightOfVespers.Agendas.OpenSeason (openSeason) where

import Arkham.Agenda.Import.Lifted
import Arkham.Cost (Cost (GroupClueCost))
import Arkham.Helpers.Modifiers (ModifierType (..), modified_, modifySelect)
import Arkham.Homebrew.NightOfVespers.CardDefs.Agendas qualified as Cards
import Arkham.Matcher

newtype OpenSeason = OpenSeason AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

openSeason :: AgendaCard OpenSeason
openSeason = agenda (2, A) OpenSeason Cards.openSeason (Static 6)

-- These printed Forced effects alter queries and costs rather than resolving
-- after a move: the entry cost must also participate in affordability checks.
instance HasModifiersFor OpenSeason where
  getModifiersFor (OpenSeason a) = do
    modified_ a GameTarget [EncounterCardDistancesAreBidirectional]
    modifySelect a UnrevealedLocation [AdditionalCostToEnter $ GroupClueCost (PerPlayer 1) Anywhere]

instance RunMessage OpenSeason where
  runMessage msg (OpenSeason attrs) = OpenSeason <$> runMessage msg attrs
