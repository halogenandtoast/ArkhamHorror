module Arkham.Event.Cards.LetGodSortThemOut (
  letGodSortThemOut,
  LetGodSortThemOut (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers

newtype LetGodSortThemOut = LetGodSortThemOut EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

letGodSortThemOut :: EventCard LetGodSortThemOut
letGodSortThemOut = event LetGodSortThemOut Cards.letGodSortThemOut

instance RunMessage LetGodSortThemOut where
  runMessage msg e@(LetGodSortThemOut attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      pushAll [AddToVictory (toTarget attrs), gameModifier attrs iid (XPModifier 1), ChooseEndTurn iid]
      pure e
    _ -> LetGodSortThemOut <$> runMessage msg attrs
