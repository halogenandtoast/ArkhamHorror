module Arkham.Asset.Assets.DanforthBrilliantStudentResolute (danforthBrilliantStudentResolute) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Capability
import Arkham.Draw.Types
import Arkham.EncounterSet (EncounterSet (Tekelili))
import Arkham.Helpers.Window (getTreacheryResolver)
import Arkham.Matcher

newtype DanforthBrilliantStudentResolute = DanforthBrilliantStudentResolute AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

danforthBrilliantStudentResolute :: AssetCard DanforthBrilliantStudentResolute
danforthBrilliantStudentResolute = allyWith DanforthBrilliantStudentResolute Cards.danforthBrilliantStudentResolute (2, 5) noSlots

instance HasAbilities DanforthBrilliantStudentResolute where
  getAbilities (DanforthBrilliantStudentResolute x) =
    [ controlled x 1 (can.draw.cards (affectsColocatedMatch You))
        $ triggered
          (ResolvesTreachery #after (affectsColocatedMatch You) "Tekeli-li")
          (assetUseCost x Secret 1 <> exhaust x)
    ]

instance RunMessage DanforthBrilliantStudentResolute where
  runMessage msg a@(DanforthBrilliantStudentResolute attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getTreacheryResolver -> iid) _ -> do
      drawCardsEdit iid (attrs.ability 1) 3 \c -> do
        c {cardDrawDiscard = Just (CardFromEncounterSet Tekelili)}
      pure a
    _ -> DanforthBrilliantStudentResolute <$> liftRunMessage msg attrs
