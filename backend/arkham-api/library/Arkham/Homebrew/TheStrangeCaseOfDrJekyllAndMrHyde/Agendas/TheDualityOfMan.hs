{- | Agenda 1, The Duality of Man.

The only agenda in the deck, and it loops. While Henry Jekyll still holds the
body, advancing hands it to Hyde, costs Hyde a horror -- the scenario's real
clock -- and flips straight back to the "a" side. Once Hyde is already out there
is nothing left to give up, and the scenario ends in defeat.
-}
module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Agendas.TheDualityOfMan (
  theDualityOfMan,
) where

import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Helpers

newtype TheDualityOfMan = TheDualityOfMan AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theDualityOfMan :: AgendaCard TheDualityOfMan
theDualityOfMan = agenda (1, A) TheDualityOfMan Cards.theDualityOfMan (Static 4)

instance RunMessage TheDualityOfMan where
  runMessage msg a@(TheDualityOfMan attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      jekyll <- selectAny henryJekyll
      if jekyll
        then do
          flipToHyde attrs
          -- The horror belongs to the face the flip leaves in play, so it waits
          -- for the flip; placing it now would put it on Jekyll on his way out.
          doStep 1 msg
          push $ RevertAgenda attrs.id
        else push R1
      pure a
    DoStep 1 (AdvanceAgenda (isSide B attrs -> True)) -> do
      placeHorrorOnHyde attrs 1
      pure a
    _ -> TheDualityOfMan <$> liftRunMessage msg attrs
