module Arkham.Asset.Assets.HaroldWalsted (
  haroldWalsted,
  HaroldWalsted (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosToken
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Trait

newtype HaroldWalsted = HaroldWalsted AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

haroldWalsted :: AssetCard HaroldWalsted
haroldWalsted =
  allyWith
    HaroldWalsted
    Cards.haroldWalsted
    (1, 1)
    ((isStoryL .~ True) . (slotsL .~ mempty))

instance HasAbilities HaroldWalsted where
  getAbilities (HaroldWalsted x) =
    [ mkAbility x 1
        $ ForcedAbility
        $ AssetLeavesPlay Timing.When
        $ AssetWithId
        $ toId x
    ]

instance HasModifiersFor HaroldWalsted where
  getModifiersFor (HaroldWalsted a) = case a.controller of
    Nothing -> pure mempty
    Just iid -> do
      getSkillTestAction >>= \case
        Just Action.Investigate -> do
          isMiskatonic <- selectAny $ locationWithInvestigator iid <> LocationWithTrait Miskatonic
          modifiedWhen_ a isMiskatonic iid [SkillModifier #intellect 2]
        _ -> pure mempty

instance RunMessage HaroldWalsted where
  runMessage msg a@(HaroldWalsted attrs) = case msg of
    UseCardAbility _ source 1 _ _
      | isSource attrs source ->
          a <$ pushAll [AddChaosToken Tablet, RemoveFromGame $ toTarget attrs]
    _ -> HaroldWalsted <$> runMessage msg attrs
