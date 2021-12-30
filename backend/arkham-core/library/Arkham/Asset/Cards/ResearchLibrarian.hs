module Arkham.Asset.Cards.ResearchLibrarian where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Attrs
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Trait
import Arkham.Zone

newtype ResearchLibrarian = ResearchLibrarian AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

researchLibrarian :: AssetCard ResearchLibrarian
researchLibrarian = ally ResearchLibrarian Cards.researchLibrarian (1, 1)

instance HasAbilities ResearchLibrarian where
  getAbilities (ResearchLibrarian x) =
    [ restrictedAbility
        x
        1
        (OwnsThis <> CanSearchDeck <> CanShuffleDeck)
        (ReactionAbility
          (AssetEntersPlay Timing.When $ AssetWithId (toId x))
          Free
        )
    ]

instance (AssetRunner env) => RunMessage env ResearchLibrarian where
  runMessage msg a@(ResearchLibrarian attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ push
      (Search
          iid
          source
          (InvestigatorTarget iid)
          [(FromDeck, ShuffleBackIn)]
          (CardWithTrait Tome)
      $ DrawFound iid 1
      )
    _ -> ResearchLibrarian <$> runMessage msg attrs
