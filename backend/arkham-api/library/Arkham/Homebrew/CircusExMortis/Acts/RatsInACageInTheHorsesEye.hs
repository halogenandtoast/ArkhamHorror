module Arkham.Homebrew.CircusExMortis.Acts.RatsInACageInTheHorsesEye (ratsInACage_006) where

import Arkham.Act.Import.Lifted
import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.CircusExMortis.Helpers (lookupRatsInACage)
import Arkham.Matcher
import Arkham.Placement

newtype RatsInACageInTheHorsesEye = RatsInACageInTheHorsesEye ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

ratsInACage_006 :: ActCard RatsInACageInTheHorsesEye
ratsInACage_006 = act (1, A) RatsInACageInTheHorsesEye Cards.ratsInACage_006 (groupClueCost $ PerPlayer 4)

instance RunMessage RatsInACageInTheHorsesEye where
  runMessage msg a@(RatsInACageInTheHorsesEye attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      for_ (lookupRatsInACage attrs) \(locationDef, token) -> do
        lid <- selectJust $ locationIs locationDef
        card <- getSetAsideCard Assets.illusoryLocus
        createAssetAt_ card (AttachedToLocation lid)
        addChaosToken token
      advanceActDeck attrs
      pure a
    _ -> RatsInACageInTheHorsesEye <$> liftRunMessage msg attrs
