module Arkham.Homebrew.TheSymphonyOfErichZann.Acts.UndreamableOrchestra (undreamableOrchestra) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.Helpers
import Arkham.Homebrew.TheSymphonyOfErichZann.Key
import Arkham.Homebrew.TheSymphonyOfErichZann.Traits (pattern Musician)
import Arkham.Matcher
import Arkham.Message.Lifted.Log

newtype UndreamableOrchestra = UndreamableOrchestra ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

undreamableOrchestra :: ActCard UndreamableOrchestra
undreamableOrchestra = act (3, A) UndreamableOrchestra Cards.undreamableOrchestra Nothing

instance HasAbilities UndreamableOrchestra where
  getAbilities (UndreamableOrchestra a) =
    -- "If there are no more [[Musician]] enemies in play, advance."
    -- The Forced that flips a defeated Musician to its Muse lives on each
    -- Musician enemy, which can see its own defeat.
    [restricted a 1 (not_ $ exists $ EnemyWithTrait Musician) $ Objective $ forced AnyWindow]

instance RunMessage UndreamableOrchestra where
  runMessage msg a@(UndreamableOrchestra attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      record YouSavedAllTheMusicians

      -- "Discard all [[Music]] treacheries currently in play."
      music <- musicTreacheriesInPlay
      for_ music \tid -> toDiscard (toSource attrs) tid
      setMusicOrder []

      -- "Advance to the back side of agenda 3a, Coda Ultimatum."
      selectEach AnyAgenda \aid -> push $ AdvanceAgendaBy aid #other
      pure a
    _ -> UndreamableOrchestra <$> liftRunMessage msg attrs
