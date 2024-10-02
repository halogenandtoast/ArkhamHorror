module Arkham.Event.Events.LetGodSortThemOut (letGodSortThemOut, LetGodSortThemOut (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Modifier

newtype LetGodSortThemOut = LetGodSortThemOut EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

letGodSortThemOut :: EventCard LetGodSortThemOut
letGodSortThemOut = event LetGodSortThemOut Cards.letGodSortThemOut

instance RunMessage LetGodSortThemOut where
  runMessage msg e@(LetGodSortThemOut attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      addToVictory attrs
      gameModifier attrs iid $ XPModifier "Let God sort them out..." 1
      endYourTurn iid
      pure e
    _ -> LetGodSortThemOut <$> liftRunMessage msg attrs
