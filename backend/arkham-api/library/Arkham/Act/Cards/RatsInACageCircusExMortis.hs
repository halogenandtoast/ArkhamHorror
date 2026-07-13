module Arkham.Act.Cards.RatsInACageCircusExMortis (
  ratsInACageCircusExMortis_005,
  ratsInACageCircusExMortis_006,
  ratsInACageCircusExMortis_007,
  ratsInACageCircusExMortis_008,
) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card.CardDef (CardDef)
import Arkham.ChaosToken
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Placement

-- Four variants of act 1; one is chosen at random during setup. Each hides the
-- Illusory Locus at a different location and, on advancing, permanently adds a
-- different chaos token (per the printed act back).
advanceRatsInACage :: ReverseQueue m => ActAttrs -> CardDef -> ChaosTokenFace -> m ()
advanceRatsInACage attrs locationDef token = do
  lid <- selectJust $ locationIs locationDef
  card <- getSetAsideCard Assets.illusoryLocusCircusExMortis
  createAssetAt_ card (AttachedToLocation lid)
  addChaosToken token
  advanceActDeck attrs

newtype RatsInACageInTheLionsDen = RatsInACageInTheLionsDen ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ratsInACageCircusExMortis_005 :: ActCard RatsInACageInTheLionsDen
ratsInACageCircusExMortis_005 =
  act (1, A) RatsInACageInTheLionsDen Cards.ratsInACageCircusExMortis_005 (groupClueCost $ PerPlayer 4)

instance RunMessage RatsInACageInTheLionsDen where
  runMessage msg a@(RatsInACageInTheLionsDen attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceRatsInACage attrs Locations.animalCagesCircusExMortis Tablet
      pure a
    _ -> RatsInACageInTheLionsDen <$> liftRunMessage msg attrs

newtype RatsInACageInTheHorsesEye = RatsInACageInTheHorsesEye ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ratsInACageCircusExMortis_006 :: ActCard RatsInACageInTheHorsesEye
ratsInACageCircusExMortis_006 =
  act (1, A) RatsInACageInTheHorsesEye Cards.ratsInACageCircusExMortis_006 (groupClueCost $ PerPlayer 4)

instance RunMessage RatsInACageInTheHorsesEye where
  runMessage msg a@(RatsInACageInTheHorsesEye attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceRatsInACage attrs Locations.carouselCircusExMortis Tablet
      pure a
    _ -> RatsInACageInTheHorsesEye <$> liftRunMessage msg attrs

newtype RatsInACageInThePrizeDisplay = RatsInACageInThePrizeDisplay ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ratsInACageCircusExMortis_007 :: ActCard RatsInACageInThePrizeDisplay
ratsInACageCircusExMortis_007 =
  act (1, A) RatsInACageInThePrizeDisplay Cards.ratsInACageCircusExMortis_007 (groupClueCost $ PerPlayer 4)

instance RunMessage RatsInACageInThePrizeDisplay where
  runMessage msg a@(RatsInACageInThePrizeDisplay attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceRatsInACage attrs Locations.gamesGalleryCircusExMortis Cultist
      pure a
    _ -> RatsInACageInThePrizeDisplay <$> liftRunMessage msg attrs

newtype RatsInACageInTheMakeupCase = RatsInACageInTheMakeupCase ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ratsInACageCircusExMortis_008 :: ActCard RatsInACageInTheMakeupCase
ratsInACageCircusExMortis_008 =
  act (1, A) RatsInACageInTheMakeupCase Cards.ratsInACageCircusExMortis_008 (groupClueCost $ PerPlayer 4)

instance RunMessage RatsInACageInTheMakeupCase where
  runMessage msg a@(RatsInACageInTheMakeupCase attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceRatsInACage attrs Locations.performerTrailersCircusExMortis Cultist
      pure a
    _ -> RatsInACageInTheMakeupCase <$> liftRunMessage msg attrs
