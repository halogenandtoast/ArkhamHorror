module Arkham.Treachery.Cards.TheInnsmouthConspiracy.RisingTide.Riptide (riptide, Riptide (..)) where

import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Location.FloodLevel
import Arkham.Matcher
import Arkham.Treachery.CardDefs.TheInnsmouthConspiracy.RisingTide qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Riptide = Riptide TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riptide :: TreacheryCard Riptide
riptide = treachery Riptide Cards.riptide

instance RunMessage Riptide where
  runMessage msg t@(Riptide attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      getFloodLevelFor iid >>= \case
        Unflooded -> gainSurge attrs
        floodLevel -> do
          sid <- getRandom
          revelationSkillTest sid iid attrs #agility
            $ Fixed (if floodLevel == FullyFlooded then 4 else 3)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      hasAssets <- selectAny $ DiscardableAsset <> assetControlledBy iid
      push
        $ if hasAssets
          then ChooseAndDiscardAsset iid (toSource attrs) AnyAsset
          else LoseResources iid (toSource attrs) n
      pure t
    _ -> Riptide <$> liftRunMessage msg attrs
