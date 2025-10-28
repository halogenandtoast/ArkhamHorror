module Arkham.Asset.Assets.ObsidianBracelet (obsidianBracelet) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype ObsidianBracelet = ObsidianBracelet AssetAttrs
  deriving anyclass (IsAsset, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

obsidianBracelet :: AssetCard ObsidianBracelet
obsidianBracelet = assetWith ObsidianBracelet Cards.obsidianBracelet $ (healthL ?~ 3) . (sanityL ?~ 3)

instance HasModifiersFor ObsidianBracelet where
  getModifiersFor (ObsidianBracelet a) = for_ a.controller \iid -> do
    modifySelect
      a
      (not_ (InvestigatorWithId iid) <> at_ (locationWithAsset a))
      [CanAssignDamageToAsset a.id, CanAssignHorrorToAsset a.id]
    modifySelf a [CannotBeDamagedBySourcesExcept $ SourceIsTreacheryEffect AnyTreachery]
