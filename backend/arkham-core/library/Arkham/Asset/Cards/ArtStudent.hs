module Arkham.Asset.Cards.ArtStudent (
  artStudent,
  ArtStudent (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Discover
import Arkham.Matcher

newtype ArtStudent = ArtStudent AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

artStudent :: AssetCard ArtStudent
artStudent = ally ArtStudent Cards.artStudent (1, 2)

instance HasAbilities ArtStudent where
  getAbilities (ArtStudent x) =
    [ controlledAbility x 1 (exists (You <> InvestigatorCanDiscoverCluesAt YourLocation))
        $ freeReaction (AssetEntersPlay #when $ AssetWithId (toId x))
    ]

instance RunMessage ArtStudent where
  runMessage msg a@(ArtStudent attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ discoverAtYourLocation iid (toAbilitySource attrs 1) 1
      pure a
    _ -> ArtStudent <$> runMessage msg attrs
