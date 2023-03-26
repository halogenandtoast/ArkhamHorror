module Arkham.Act.Cards.HiddenAgendas
  ( HiddenAgendas(..)
  , hiddenAgendas
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype HiddenAgendas = HiddenAgendas ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hiddenAgendas :: ActCard HiddenAgendas
hiddenAgendas = act (1, A) HiddenAgendas Cards.hiddenAgendas Nothing

instance RunMessage HiddenAgendas where
  runMessage msg a@(HiddenAgendas attrs) = case msg of
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      push $ advanceActDeck attrs
      pure a
    _ -> HiddenAgendas <$> runMessage msg attrs
