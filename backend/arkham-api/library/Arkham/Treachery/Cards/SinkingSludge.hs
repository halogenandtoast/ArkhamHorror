module Arkham.Treachery.Cards.SinkingSludge (sinkingSludge) where

import Arkham.Helpers.Location (getLocationOf)
import Arkham.Matcher
import Arkham.Trait (Trait (Bog, Sunken))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SinkingSludge = SinkingSludge TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sinkingSludge :: TreacheryCard SinkingSludge
sinkingSludge = treachery SinkingSludge Cards.sinkingSludge

instance RunMessage SinkingSludge where
  runMessage msg t@(SinkingSludge attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      isSunken <- selectAny $ locationWithInvestigator iid <> LocationWithTrait Sunken
      when isSunken do
        directDamage iid attrs 1
        assets <- select $ assetControlledBy iid <> AllyAsset
        for_ assets \asset -> dealAssetDirectDamage asset attrs 1
      isBog <- selectAny $ locationWithInvestigator iid <> LocationWithTrait Bog
      when isBog do
        getLocationOf iid >>= traverse_ \lid ->
          placeTokens attrs lid #damage 1
      pure t
    _ -> SinkingSludge <$> liftRunMessage msg attrs
