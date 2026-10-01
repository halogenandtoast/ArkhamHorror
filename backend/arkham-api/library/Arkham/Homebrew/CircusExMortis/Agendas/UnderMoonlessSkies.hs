module Arkham.Homebrew.CircusExMortis.Agendas.UnderMoonlessSkies (underMoonlessSkies) where

import Arkham.Ability
import Arkham.Act.Types (Field (ActDeckId))
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Card (getCardEntityTarget)
import Arkham.Homebrew.CircusExMortis.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.CircusExMortis.Key
import Arkham.Homebrew.CircusExMortis.Traits (pattern Tainted)
import Arkham.Matcher
import Arkham.Message.Lifted.Log (record)
import Arkham.Message.Lifted.Move (moveToward)
import Arkham.Projection

newtype UnderMoonlessSkies = UnderMoonlessSkies AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

underMoonlessSkies :: AgendaCard UnderMoonlessSkies
underMoonlessSkies = agenda (1, A) UnderMoonlessSkies Cards.underMoonlessSkies (Static 3)

{- | "each player card attached to a [[Tainted]] location". Attachments live on the
asset\/event\/skill entity rather than on the location, so this is read off the
card's placement ('CardIsAttachedToLocation') rather than by walking the location.
-}
attachedToTainted :: ExtendedCardMatcher
attachedToTainted = CardIsAttachedToLocation (LocationWithTrait Tainted) <> basic IsPlayerCard

instance HasAbilities UnderMoonlessSkies where
  getAbilities (UnderMoonlessSkies a) =
    -- With no such attachment in play the Forced cannot change the game state, so it
    -- is not triggered at all rather than resolving to a no-op.
    [restricted a 1 (exists attachedToTainted) $ forced $ RoundEnds #when]

instance RunMessage UnderMoonlessSkies where
  runMessage msg a@(UnderMoonlessSkies attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      cards <- select attachedToTainted
      for_ cards $ getCardEntityTarget >=> traverse_ (toDiscard attrs)
      pure a
    -- Back, The Woods Quake: "Move Shub-Niggurath once toward Silent Clearing."
    -- Whether it arrived can only be read after the move resolves, so the branch
    -- waits a step.
    AdvanceAgenda (isSide B attrs -> True) -> do
      selectEach (enemyIs Enemies.shubNiggurath) \shub ->
        moveToward shub (locationIs Locations.silentClearing)
      doStep 1 msg
      pure a
    DoStep 1 (AdvanceAgenda (isSide B attrs -> True)) -> do
      arrived <-
        selectAny $ enemyIs Enemies.shubNiggurath <> EnemyAt (locationIs Locations.silentClearing)
      if not arrived
        then revertAgenda attrs
        else do
          -- "Otherwise, remove the act and agenda from the game and advance to the
          -- set-aside The Prophecy Unfulfilled; it is both the current act and the
          -- current agenda." The Prophecy cards are implemented as agendas only, so
          -- advancing the agenda deck to it leaves the act deck empty.
          selectEach AnyAct \aid -> do
            deckId <- field ActDeckId aid
            push $ RemoveCompletedActFromGame deckId aid
          advanceToAgendaA attrs Cards.theProphecyUnfulfilled
          record TheProphecyWasUnfulfilled
      pure a
    _ -> UnderMoonlessSkies <$> liftRunMessage msg attrs
