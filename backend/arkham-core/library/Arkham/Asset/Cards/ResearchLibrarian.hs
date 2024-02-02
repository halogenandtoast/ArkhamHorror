module Arkham.Asset.Cards.ResearchLibrarian where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Trait

newtype ResearchLibrarian = ResearchLibrarian AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

researchLibrarian :: AssetCard ResearchLibrarian
researchLibrarian = ally ResearchLibrarian Cards.researchLibrarian (1, 1)

instance HasAbilities ResearchLibrarian where
  getAbilities (ResearchLibrarian x) =
    [ controlledAbility x 1 (CanSearchDeck <> CanShuffleDeck)
        $ freeReaction (AssetEntersPlay #when $ AssetWithId (toId x))
    ]

instance RunMessage ResearchLibrarian where
  runMessage msg a@(ResearchLibrarian attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      push $ search iid source iid [fromDeck] (CardWithTrait Tome) (DrawFound iid 1)
      pure a
    _ -> ResearchLibrarian <$> runMessage msg attrs
