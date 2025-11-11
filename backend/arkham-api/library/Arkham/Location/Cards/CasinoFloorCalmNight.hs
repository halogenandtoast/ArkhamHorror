module Arkham.Location.Cards.CasinoFloorCalmNight (casinoFloorCalmNight) where

import Arkham.Ability
import Arkham.Asset.Types (Field (AssetCard))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (AssetCard)
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.FortuneAndFolly.Helpers

newtype CasinoFloorCalmNight = CasinoFloorCalmNight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

casinoFloorCalmNight :: LocationCard CasinoFloorCalmNight
casinoFloorCalmNight = symbolLabel $ location CasinoFloorCalmNight Cards.casinoFloorCalmNight 2 (Static 0)

instance HasAbilities CasinoFloorCalmNight where
  getAbilities (CasinoFloorCalmNight a) =
    extendRevealed
      a
      [ scenarioI18n $ withI18nTooltip "casinoFloorCalmNight.resign" $ locationResignAction a
      , playerLimit PerTurn
          $ restricted
            a
            1
            ( Here
                <> oneOf
                  [ exists (InHandOf NotForPlay You <> #asset)
                  , exists (AssetInPlayAreaOf You <> AssetCanLeavePlayByNormalMeans)
                  ]
            )
            actionAbility
      ]

instance RunMessage CasinoFloorCalmNight where
  runMessage msg l@(CasinoFloorCalmNight attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      cards <- select $ InHandOf NotForPlay (InvestigatorWithId iid) <> #asset
      assets <-
        selectWithField AssetCard
          $ AssetInPlayAreaOf (InvestigatorWithId iid)
          <> AssetCanLeavePlayByNormalMeans
      chooseOneM iid do
        targets cards $ placeUnderneath attrs . only
        for_ assets \(asset, card) ->
          targeting asset $ placeUnderneath attrs (only card)
      pure l
    _ -> CasinoFloorCalmNight <$> liftRunMessage msg attrs
