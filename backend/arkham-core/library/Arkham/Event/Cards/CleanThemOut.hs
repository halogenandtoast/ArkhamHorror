module Arkham.Event.Cards.CleanThemOut (cleanThemOut, CleanThemOut (..)) where

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Fight
import Arkham.Prelude

newtype CleanThemOut = CleanThemOut EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cleanThemOut :: EventCard CleanThemOut
cleanThemOut = event CleanThemOut Cards.cleanThemOut

instance RunMessage CleanThemOut where
  runMessage msg e@(CleanThemOut attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      chooseFight <- toMessage <$> mkChooseFight iid attrs
      pushAll [TakeResources iid 2 (toSource attrs) False, chooseFight]
      pure e
    _ -> CleanThemOut <$> runMessage msg attrs
