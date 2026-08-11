module Arkham.Homebrew.DarkMatter.Acts.ElbrusStation (elbrusStation) where

import Arkham.Act.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Cards

newtype ElbrusStation = ElbrusStation ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

elbrusStation :: ActCard ElbrusStation
elbrusStation = act (1, A) ElbrusStation Cards.elbrusStation (groupClueCost $ PerPlayer 3)

instance RunMessage ElbrusStation where
  runMessage msg a@(ElbrusStation attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> ElbrusStation <$> liftRunMessage msg attrs
