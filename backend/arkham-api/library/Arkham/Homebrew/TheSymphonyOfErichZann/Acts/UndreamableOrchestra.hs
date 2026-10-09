module Arkham.Homebrew.TheSymphonyOfErichZann.Acts.UndreamableOrchestra (undreamableOrchestra) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Agenda.Sequence qualified as Agenda
import Arkham.Card
import Arkham.Enemy.Types (Field (EnemyCard))
import Arkham.Helpers.Story (readStory)
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.TheSymphonyOfErichZann.Helpers
import Arkham.Homebrew.TheSymphonyOfErichZann.Key
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.Projection
import Arkham.Window (Window, windowType)
import Arkham.Window qualified as Window

newtype UndreamableOrchestra = UndreamableOrchestra ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

undreamableOrchestra :: ActCard UndreamableOrchestra
undreamableOrchestra = act (3, A) UndreamableOrchestra Cards.undreamableOrchestra Nothing

instance HasAbilities UndreamableOrchestra where
  getAbilities (UndreamableOrchestra a) =
    [ {- "Forced - After a [[Musician]] enemy is defeated, flip it over and
      resolve its text on the other side." Ahead of the objective, which shares
      the same window when the last Musician is the one defeated.

      `musicianEnemies` is the scenario's own roster rather than the
      [[Musician]] trait -- see its note in Helpers. -}
      mkAbility a 1 $ forced $ EnemyDefeated #after Anyone ByAny musicianEnemies
    , -- "Objective - If there are no more [[Musician]] enemies in play, advance."
      onlyOnce $ restricted a 2 (not_ $ exists musicianEnemies) $ Objective $ forced AnyWindow
    ]

-- | The enemy the defeat window this ability triggered on is about.
getDefeatedEnemy :: [Window] -> Maybe EnemyId
getDefeatedEnemy = \case
  [] -> Nothing
  ((windowType -> Window.EnemyDefeated _ _ eid) : _) -> Just eid
  (_ : rest) -> getDefeatedEnemy rest

instance RunMessage UndreamableOrchestra where
  runMessage msg a@(UndreamableOrchestra attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getDefeatedEnemy -> Just eid) _ -> do
      {- "...flip it over and resolve its text on the other side." Every
      Musician's def names its Muse as `cdOtherSide`, so the flip is read off
      the card itself rather than from a table kept in step by hand. -}
      card <- field EnemyCard eid
      for_ (toCardDef card).otherSide \code -> for_ (lookupCardDef code) \muse -> do
        museCard <- fetchCard muse
        readStory iid museCard muse
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      record YouSavedAllTheMusicians

      -- "Discard all [[Music]] treacheries currently in play."
      music <- musicTreacheriesInPlay
      for_ music $ toDiscard (toSource attrs)
      setMusicOrder []

      {- "Advance to the back side of agenda 3a, Coda Ultimatum." Flipping
      whatever agenda happens to be current is not the same thing -- the act can
      be finished while the deck is still on Overture or Crescendo. This names
      agenda 3a, so the deck skips forward to it and *then* turns it over. -}
      push $ AdvanceToAgenda 1 Agendas.opusMagnum Agenda.B (toSource attrs)
      pure a
    _ -> UndreamableOrchestra <$> liftRunMessage msg attrs
