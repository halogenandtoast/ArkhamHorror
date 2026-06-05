module Arkham.Act.Cards.BlackwatersBaneEpicMultiplayer (blackwatersBaneEpicMultiplayer) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype BlackwatersBaneEpicMultiplayer = BlackwatersBaneEpicMultiplayer ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blackwatersBaneEpicMultiplayer :: ActCard BlackwatersBaneEpicMultiplayer
blackwatersBaneEpicMultiplayer = act (3, A) BlackwatersBaneEpicMultiplayer Cards.blackwatersBaneEpicMultiplayer Nothing

instance RunMessage BlackwatersBaneEpicMultiplayer where
  runMessage msg (BlackwatersBaneEpicMultiplayer attrs) = BlackwatersBaneEpicMultiplayer <$> runMessage msg attrs
