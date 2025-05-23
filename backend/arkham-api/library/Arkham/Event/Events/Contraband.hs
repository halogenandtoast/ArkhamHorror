module Arkham.Event.Events.Contraband (contraband) where

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

      chooseOneM iid do
        for_ assets \(useType', assetId, assetUseCount) ->
          targeting assetId $ addUses attrs assetId useType' assetUseCount
      pure e
    _ -> Contraband <$> liftRunMessage msg attrs
