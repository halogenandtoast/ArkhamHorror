module Arkham.Asset.Cards.ObsidianBracelet (obsidianBracelet, ObsidianBracelet (..)) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Location (onSameLocation)
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype ObsidianBracelet = ObsidianBracelet AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

obsidianBracelet :: AssetCard ObsidianBracelet
obsidianBracelet = assetWith ObsidianBracelet Cards.obsidianBracelet $ (healthL ?~ 3) . (sanityL ?~ 3)

instance HasModifiersFor ObsidianBracelet where
  getModifiersFor (InvestigatorTarget iid) (ObsidianBracelet a) | not (controlledBy a iid) = do
    maybeModified a do
      liftGuardM $ onSameLocation iid a
      pure [CanAssignDamageToAsset a.id, CanAssignHorrorToAsset a.id]
  getModifiersFor target (ObsidianBracelet a) | isTarget a target = do
    modified a [CannotBeDamagedBySourcesExcept $ SourceIsTreacheryEffect AnyTreachery]
  getModifiersFor _ _ = pure []

instance RunMessage ObsidianBracelet where
  runMessage msg (ObsidianBracelet attrs) = runQueueT $ case msg of
    _ -> ObsidianBracelet <$> liftRunMessage msg attrs
