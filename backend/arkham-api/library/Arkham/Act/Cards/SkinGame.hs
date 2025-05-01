module Arkham.Act.Cards.SkinGame (skinGame) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Helpers.Campaign
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Placement
import Arkham.Trait

newtype SkinGame = SkinGame ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

skinGame :: ActCard SkinGame
skinGame =
  act (2, A) SkinGame Cards.skinGame
    $ Just
    $ GroupClueCost (PerPlayer 2)
    $ LocationWithTitle "VIP Area"

instance RunMessage SkinGame where
  runMessage msg a@(SkinGame attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      completedExtracurricularActivity <- anyM completedScenario ["02041", "51012"]
      lead <- getLead
      if completedExtracurricularActivity
        then do
          cloverClubBar <- getJustLocationByName "Clover Club Bar"
          createAssetAt_ Assets.peterClover (AtLocation cloverClubBar)
          findEncounterCardIn lead attrs (card_ $ #enemy <> withTrait Abomination) (#deck : allOutOfPlayZones)
          advanceToAct attrs Acts.fold A
        else do
          vipArea <- getJustLocationByName "VIP Area"
          createAssetAt_ Assets.drFrancisMorgan (AtLocation vipArea)
          advanceToAct attrs Acts.allIn A
      pure a
    FoundEncounterCard _ (isTarget attrs -> True) ec -> do
      cloverClubBar <- getJustLocationByName "Clover Club Bar"
      spawnEnemyAt_ ec cloverClubBar
      pure a
    _ -> SkinGame <$> liftRunMessage msg attrs
