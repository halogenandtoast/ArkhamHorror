module Arkham.Event.Cards.Contraband2 (contraband2, Contraband2 (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Message (drawCards)
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Query (getPlayer)
import Arkham.Helpers.Use
import Arkham.Matcher

newtype Contraband2 = Contraband2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

contraband2 :: EventCard Contraband2
contraband2 = event Contraband2 Cards.contraband2

instance RunMessage Contraband2 where
  runMessage msg e@(Contraband2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      investigators <- select $ affectsOthers $ colocatedWith iid
      assets <- concatForM [Ammo, Supply] \k -> do
        select (AssetWithUseType k <> AssetNotAtUseLimit <> mapOneOf assetControlledBy investigators)
          >>= traverse (\aid -> (k,aid,) <$> getAssetUses k aid)

      let drawing = drawCards iid attrs 1

      player <- getPlayer iid
      chooseOne
        iid
        [ Label
            "Place 2 ammo or supply tokens on that asset and draw 1 card."
            [ Msg.chooseOne
                player
                [ targetLabel assetId [AddUses (toSource attrs) assetId useType' 2, drawing]
                | (useType', assetId, _) <- assets
                ]
            ]
        , Label
            "Double the number of ammo or supply tokens on that asset."
            [ Msg.chooseOne
                player
                [ targetLabel assetId [AddUses (toSource attrs) assetId useType' assetUseCount]
                | (useType', assetId, assetUseCount) <- assets
                ]
            ]
        ]
      pure e
    _ -> Contraband2 <$> liftRunMessage msg attrs
