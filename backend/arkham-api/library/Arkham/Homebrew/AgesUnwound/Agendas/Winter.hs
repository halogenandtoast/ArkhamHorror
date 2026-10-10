module Arkham.Homebrew.AgesUnwound.Agendas.Winter (winter) where

import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Homebrew.AgesUnwound.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Treacheries
import Arkham.Homebrew.AgesUnwound.Missions.Helpers (putTaskIntoPlay)

newtype Winter = Winter AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

winter :: AgendaCard Winter
winter = agenda (2, A) Winter Cards.winter (Static 4)

-- | "If there is only 1 investigator in the game, this agenda gets +1 doom threshold."
instance HasModifiersFor Winter where
  getModifiersFor (Winter a) = do
    n <- getPlayerCount
    modifySelf a [DoomThresholdModifier 1 | n == 1]

instance RunMessage Winter where
  runMessage msg a@(Winter attrs) = runQueueT $ case msg of
    {- "__A Curious Expedition__ - Put the set-aside The Tunguska Event treachery
    into play next to the act deck. Shuffle the encounter discard pile into the
    encounter deck."

    The Tunguska Event prints no __Revelation__, so nothing is resolved -- the
    Task simply takes up its place beside the act deck. -}
    AdvanceAgenda (isSide B attrs -> True) -> do
      putTaskIntoPlay Treacheries.theTunguskaEvent
      shuffleEncounterDiscardBackIn
      advanceAgendaDeck attrs
      pure a
    _ -> Winter <$> liftRunMessage msg attrs
