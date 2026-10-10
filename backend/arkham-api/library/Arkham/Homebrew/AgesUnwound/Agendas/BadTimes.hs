module Arkham.Homebrew.AgesUnwound.Agendas.BadTimes (badTimes) where

import Arkham.Agenda.Import.Lifted
import Arkham.Card
import Arkham.Homebrew.AgesUnwound.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.Unstuck.Helpers
import Arkham.Homebrew.AgesUnwound.Traits (pattern Adrift)
import Arkham.Helpers.Query (getSetAsideCardMaybe)
import Arkham.Location.Types qualified as Field
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (Swap))
import Arkham.Projection

newtype BadTimes = BadTimes AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

badTimes :: AgendaCard BadTimes
badTimes = agenda (3, A) BadTimes Cards.badTimes (Static 5)

instance RunMessage BadTimes where
  runMessage msg a@(BadTimes attrs) = runQueueT $ case msg of
    {- "Reality Changes. For each Adrift location occupied by an investigator,
    find the version of that location that was removed from the game and swap
    them (all tokens and cards at the former location are now considered to be
    at the new location). Do not place clues on the new locations as they are
    revealed. ..."

    `ReplaceLocation .. Swap` is exactly that sentence: the location id, its
    tokens, its cards underneath, its label, its ring edges, its revealed state
    and its position all carry over, and -- unlike `DefaultReplace` -- it pushes
    no `PlacedLocation`, so no clues are placed. The outgoing printing goes back
    to the set-aside pool, which keeps the removed-version pool at eight and
    lets a later swap come back the other way. -}
    AdvanceAgenda (isSide B attrs -> True) -> do
      occupied <- select $ LocationWithTrait Adrift <> LocationWithInvestigator Anyone
      for_ occupied \lid -> do
        current <- field Field.LocationCard lid
        for_ (otherPrinting $ toCardDef current) \def ->
          getSetAsideCardMaybe def >>= traverse_ \replacement -> do
            obtainCard replacement
            push $ ReplaceLocation lid replacement Swap
            push $ SetAsideCards [current]

      tumbleOutOfTime attrs
      advanceAgendaDeck attrs
      placeDoomOnAgendaAndCheckAdvance 1
      pure a
    _ -> BadTimes <$> liftRunMessage msg attrs
