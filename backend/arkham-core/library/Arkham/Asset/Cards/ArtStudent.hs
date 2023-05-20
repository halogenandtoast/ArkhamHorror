module Arkham.Asset.Cards.ArtStudent (
  artStudent,
  ArtStudent (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype ArtStudent = ArtStudent AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

artStudent :: AssetCard ArtStudent
artStudent = ally ArtStudent Cards.artStudent (1, 2)

instance HasAbilities ArtStudent where
  getAbilities (ArtStudent x) =
    [ restrictedAbility
        x
        1
        ( ControlsThis
            <> InvestigatorExists
              (You <> InvestigatorCanDiscoverCluesAt YourLocation)
        )
        ( ReactionAbility
            (AssetEntersPlay Timing.When $ AssetWithId (toId x))
            Free
        )
    ]

instance RunMessage ArtStudent where
  runMessage msg a@(ArtStudent attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      a <$ push (InvestigatorDiscoverCluesAtTheirLocation iid (toAbilitySource attrs 1) 1 Nothing)
    _ -> ArtStudent <$> runMessage msg attrs
