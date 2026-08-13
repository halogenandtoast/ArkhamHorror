module Arkham.Homebrew.DarkMatter.Agendas.MoonsOfSaturn (moonsOfSaturn) where

import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (addImpendingDoom, scenarioI18n)
import Arkham.Phase

-- | "Do not draw cards from the encounter deck during the mythos phase."
newtype MoonsOfSaturn = MoonsOfSaturn AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moonsOfSaturn :: AgendaCard MoonsOfSaturn
moonsOfSaturn = agenda (1, A) MoonsOfSaturn Cards.moonsOfSaturn (Static 3)

instance HasModifiersFor MoonsOfSaturn where
  getModifiersFor (MoonsOfSaturn a) =
    modified_ a (PhaseTarget #mythos) [SkipMythosPhaseStep EachInvestigatorDrawsEncounterCardStep]

instance RunMessage MoonsOfSaturn where
  runMessage msg a@(MoonsOfSaturn attrs) = runQueueT $ case msg of
    {- Agenda 1b, "Conspiracy Theory":

    "Add 1 tally mark under 'Impending Doom' in your Campaign Log.
    Flip this agenda back to agenda 1a." -}
    AdvanceAgenda (isSide B attrs -> True) -> do
      scenarioI18n "strangeMoons" $ scope "agenda1b" do
        flavor $ setTitle "title" >> p "body"
      addImpendingDoom 1
      revertAgenda attrs
      pure a
    _ -> MoonsOfSaturn <$> liftRunMessage msg attrs
