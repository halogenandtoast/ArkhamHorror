module Arkham.Event.Cards.Contraband (contraband, Contraband (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Use
import Arkham.Matcher

newtype Contraband = Contraband EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

contraband :: EventCard Contraband
contraband = event Contraband Cards.contraband

instance RunMessage Contraband where
  runMessage msg e@(Contraband attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      investigators <- select $ affectsOthers $ colocatedWith iid
      assets <- concatForM [Ammo, Supply] \k -> do
        select (AssetWithUseType k <> AssetNotAtUseLimit <> mapOneOf assetControlledBy investigators)
          >>= traverse (\aid -> (k,aid,) <$> getAssetUses k aid)

      chooseOne
        iid
        [ targetLabel assetId [AddUses assetId useType' assetUseCount]
        | (useType', assetId, assetUseCount) <- assets
        ]
      pure e
    _ -> Contraband <$> lift (runMessage msg attrs)
