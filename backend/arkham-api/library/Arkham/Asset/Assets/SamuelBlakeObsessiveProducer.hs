module Arkham.Asset.Assets.SamuelBlakeObsessiveProducer (samuelBlakeObsessiveProducer) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

-- All of this card is implemented in Arkham.Investigator.Cards.LolaHayesParallel

newtype SamuelBlakeObsessiveProducer = SamuelBlakeObsessiveProducer AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

samuelBlakeObsessiveProducer :: AssetCard SamuelBlakeObsessiveProducer
samuelBlakeObsessiveProducer = ally SamuelBlakeObsessiveProducer Cards.samuelBlakeObsessiveProducer (3, 0)

instance RunMessage SamuelBlakeObsessiveProducer where
  runMessage msg (SamuelBlakeObsessiveProducer attrs) = runQueueT $ case msg of
    _ -> SamuelBlakeObsessiveProducer <$> liftRunMessage msg attrs
