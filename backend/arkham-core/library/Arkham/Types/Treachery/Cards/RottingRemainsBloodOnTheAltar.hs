module Arkham.Types.Treachery.Cards.RottingRemainsBloodOnTheAltar
  ( rottingRemainsBloodOnTheAltar
  , RottingRemainsBloodOnTheAltar(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Cards.RottingRemains

newtype RottingRemainsBloodOnTheAltar = RottingRemainsBloodOnTheAltar RottingRemains
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor env, HasAbilities env)

rottingRemainsBloodOnTheAltar :: TreacheryCard RottingRemainsBloodOnTheAltar
rottingRemainsBloodOnTheAltar = treachery
  (RottingRemainsBloodOnTheAltar . RottingRemains)
  Cards.rottingRemainsBloodOnTheAltar

instance RunMessage env RottingRemainsBloodOnTheAltar where
  runMessage msg (RottingRemainsBloodOnTheAltar inner) =
    RottingRemainsBloodOnTheAltar <$> runMessage msg inner
