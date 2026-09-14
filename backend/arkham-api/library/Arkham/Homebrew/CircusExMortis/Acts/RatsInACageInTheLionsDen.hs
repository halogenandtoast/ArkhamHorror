module Arkham.Homebrew.CircusExMortis.Acts.RatsInACageInTheLionsDen (ratsInACage_005) where

import Arkham.Act.Import.Lifted
import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.CircusExMortis.Helpers (lookupRatsInACage)
import Arkham.Matcher
import Arkham.Placement

newtype RatsInACageInTheLionsDen = RatsInACageInTheLionsDen ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

ratsInACage_005 :: ActCard RatsInACageInTheLionsDen
ratsInACage_005 = act (1, A) RatsInACageInTheLionsDen Cards.ratsInACage_005 (groupClueCost $ PerPlayer 4)

instance RunMessage RatsInACageInTheLionsDen where
  runMessage msg a@(RatsInACageInTheLionsDen attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      for_ (lookupRatsInACage attrs) \(locationDef, token) -> do
        lid <- selectJust $ locationIs locationDef
        card <- getSetAsideCard Assets.illusoryLocus
        createAssetAt_ card $ AttachedToLocation lid
        addChaosToken token
      advanceActDeck attrs
      pure a
    _ -> RatsInACageInTheLionsDen <$> liftRunMessage msg attrs
