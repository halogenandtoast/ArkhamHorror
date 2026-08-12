module Arkham.Homebrew.CircusExMortis.Acts.RatsInACage (
  ratsInACage_005,
  ratsInACage_006,
  ratsInACage_007,
  ratsInACage_008,
) where

import Arkham.Act.Import.Lifted
import Arkham.Card (toCardDef)
import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.CircusExMortis.Helpers (lookupRatsInACage)
import Arkham.Matcher
import Arkham.Placement

advanceRatsInACage :: ReverseQueue m => ActAttrs -> m ()
advanceRatsInACage attrs = do
  for_ (lookupRatsInACage $ toCardDef attrs) \(locationDef, token) -> do
    lid <- selectJust $ locationIs locationDef
    card <- getSetAsideCard Assets.illusoryLocus
    createAssetAt_ card (AttachedToLocation lid)
    addChaosToken token
  advanceActDeck attrs

newtype RatsInACageInTheLionsDen = RatsInACageInTheLionsDen ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ratsInACage_005 :: ActCard RatsInACageInTheLionsDen
ratsInACage_005 =
  act (1, A) RatsInACageInTheLionsDen Cards.ratsInACage_005 (groupClueCost $ PerPlayer 4)

instance RunMessage RatsInACageInTheLionsDen where
  runMessage msg a@(RatsInACageInTheLionsDen attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceRatsInACage attrs
      pure a
    _ -> RatsInACageInTheLionsDen <$> liftRunMessage msg attrs

newtype RatsInACageInTheHorsesEye = RatsInACageInTheHorsesEye ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ratsInACage_006 :: ActCard RatsInACageInTheHorsesEye
ratsInACage_006 =
  act (1, A) RatsInACageInTheHorsesEye Cards.ratsInACage_006 (groupClueCost $ PerPlayer 4)

instance RunMessage RatsInACageInTheHorsesEye where
  runMessage msg a@(RatsInACageInTheHorsesEye attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceRatsInACage attrs
      pure a
    _ -> RatsInACageInTheHorsesEye <$> liftRunMessage msg attrs

newtype RatsInACageInThePrizeDisplay = RatsInACageInThePrizeDisplay ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ratsInACage_007 :: ActCard RatsInACageInThePrizeDisplay
ratsInACage_007 =
  act (1, A) RatsInACageInThePrizeDisplay Cards.ratsInACage_007 (groupClueCost $ PerPlayer 4)

instance RunMessage RatsInACageInThePrizeDisplay where
  runMessage msg a@(RatsInACageInThePrizeDisplay attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceRatsInACage attrs
      pure a
    _ -> RatsInACageInThePrizeDisplay <$> liftRunMessage msg attrs

newtype RatsInACageInTheMakeupCase = RatsInACageInTheMakeupCase ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ratsInACage_008 :: ActCard RatsInACageInTheMakeupCase
ratsInACage_008 =
  act (1, A) RatsInACageInTheMakeupCase Cards.ratsInACage_008 (groupClueCost $ PerPlayer 4)

instance RunMessage RatsInACageInTheMakeupCase where
  runMessage msg a@(RatsInACageInTheMakeupCase attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceRatsInACage attrs
      pure a
    _ -> RatsInACageInTheMakeupCase <$> liftRunMessage msg attrs
