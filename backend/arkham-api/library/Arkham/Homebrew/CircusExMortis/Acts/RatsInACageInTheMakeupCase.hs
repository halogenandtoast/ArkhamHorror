module Arkham.Homebrew.CircusExMortis.Acts.RatsInACageInTheMakeupCase (ratsInACage_008) where

import Arkham.Act.Import.Lifted
import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.CircusExMortis.Helpers (lookupRatsInACage)
import Arkham.Matcher
import Arkham.Placement

newtype RatsInACageInTheMakeupCase = RatsInACageInTheMakeupCase ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

ratsInACage_008 :: ActCard RatsInACageInTheMakeupCase
ratsInACage_008 = act (1, A) RatsInACageInTheMakeupCase Cards.ratsInACage_008 (groupClueCost $ PerPlayer 4)

instance RunMessage RatsInACageInTheMakeupCase where
  runMessage msg a@(RatsInACageInTheMakeupCase attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      for_ (lookupRatsInACage attrs) \(locationDef, token) -> do
        lid <- selectJust $ locationIs locationDef
        card <- getSetAsideCard Assets.illusoryLocus
        createAssetAt_ card (AttachedToLocation lid)
        addChaosToken token
      advanceActDeck attrs
      pure a
    _ -> RatsInACageInTheMakeupCase <$> liftRunMessage msg attrs
