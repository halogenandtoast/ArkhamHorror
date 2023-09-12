module Arkham.Asset.Cards.ResearchLibrarian where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Trait

newtype ResearchLibrarian = ResearchLibrarian AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

researchLibrarian :: AssetCard ResearchLibrarian
researchLibrarian = ally ResearchLibrarian Cards.researchLibrarian (1, 1)

instance HasAbilities ResearchLibrarian where
  getAbilities (ResearchLibrarian x) =
    [ restrictedAbility x 1 (ControlsThis <> CanSearchDeck <> CanShuffleDeck)
        $ ReactionAbility
          (AssetEntersPlay Timing.When $ AssetWithId (toId x))
          Free
    ]

instance RunMessage ResearchLibrarian where
  runMessage msg a@(ResearchLibrarian attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push
        $ Search iid (toAbilitySource attrs 1) (toTarget iid) [(FromDeck, ShuffleBackIn)] (CardWithTrait Tome)
        $ DrawFound iid 1
      pure a
    _ -> ResearchLibrarian <$> runMessage msg attrs
