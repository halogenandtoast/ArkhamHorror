module Arkham.Asset.Assets.ObsidianBracelet (obsidianBracelet, ObsidianBracelet (..)) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype ObsidianBracelet = ObsidianBracelet AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

obsidianBracelet :: AssetCard ObsidianBracelet
obsidianBracelet = assetWith ObsidianBracelet Cards.obsidianBracelet $ (healthL ?~ 3) . (sanityL ?~ 3)

instance HasModifiersFor ObsidianBracelet where
  getModifiersFor (ObsidianBracelet a) = case a.controller of
    Nothing -> pure mempty
    Just iid -> do
      others <-
        modifySelect
          a
          (not_ (InvestigatorWithId iid) <> at_ (locationWithAsset a))
          [CanAssignDamageToAsset a.id, CanAssignHorrorToAsset a.id]
      self <- modifySelf a [CannotBeDamagedBySourcesExcept $ SourceIsTreacheryEffect AnyTreachery]
      pure $ others <> self

instance RunMessage ObsidianBracelet where
  runMessage msg (ObsidianBracelet attrs) = runQueueT $ case msg of
    _ -> ObsidianBracelet <$> liftRunMessage msg attrs
