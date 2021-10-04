module Arkham.Types.Asset.Cards.ResearchLibrarian where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Matcher
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing
import Arkham.Types.Trait
import Arkham.Types.Zone

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
        OwnsThis
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
