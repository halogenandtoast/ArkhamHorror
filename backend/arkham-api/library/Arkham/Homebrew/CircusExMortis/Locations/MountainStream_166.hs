module Arkham.Homebrew.CircusExMortis.Locations.MountainStream_166 (mountainStream_166) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype MountainStream_166 = MountainStream_166 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mountainStream_166 :: LocationCard MountainStream_166
mountainStream_166 = location MountainStream_166 Cards.mountainStream_166 1 (PerPlayer 1)

instance HasAbilities MountainStream_166 where
  getAbilities (MountainStream_166 a) =
    extendRevealed1 a
      $ restricted a 1 (exists $ AssetControlledBy You <> AssetReady)
      $ forced
      $ DiscoverClues #after You (be a) (atLeast 1)

instance RunMessage MountainStream_166 where
  runMessage msg l@(MountainStream_166 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assets <- select $ assetControlledBy iid <> AssetReady
      -- "with printed abilities, if possible" is a preference, not a filter: an asset with
      -- printed abilities is only skipped when there are not two of them to exhaust.
      (withAbilities, others) <- partitionM (selectAny . AssetAbility . AssetWithId) assets
      let (mustExhaust, pool) =
            if length withAbilities >= 2 then ([], withAbilities) else (withAbilities, others)
      for_ mustExhaust $ exhaustWith (attrs.ability 1)
      chooseNM iid (min (2 - length mustExhaust) (length pool))
        $ targets pool
        $ exhaustWith (attrs.ability 1)
      pure l
    _ -> MountainStream_166 <$> liftRunMessage msg attrs
