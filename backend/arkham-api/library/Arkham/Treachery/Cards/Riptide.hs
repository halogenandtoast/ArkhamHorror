module Arkham.Treachery.Cards.Riptide (riptide, Riptide (..)) where

import Arkham.Location.FloodLevel
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Riptide = Riptide TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riptide :: TreacheryCard Riptide
riptide = treachery Riptide Cards.riptide

instance RunMessage Riptide where
  runMessage msg t@(Riptide attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      selectOne (locationWithInvestigator iid <> FloodedLocation) >>= \case
        Just lid -> do
          sid <- getRandom
          fieldWithDefault Unflooded LocationFloodLevel lid >>= \case
            Unflooded -> gainSurge attrs -- should be covered below
            PartiallyFlooded -> revelationSkillTest sid iid attrs #agility (Fixed 3)
            FullyFlooded -> revelationSkillTest sid iid attrs #agility (Fixed 4)
        Nothing -> gainSurge attrs
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      hasAssets <- selectAny $ DiscardableAsset <> assetControlledBy iid
      push
        $ if hasAssets
          then ChooseAndDiscardAsset iid (toSource attrs) AnyAsset
          else LoseResources iid (toSource attrs) n
      pure t
    _ -> Riptide <$> liftRunMessage msg attrs
