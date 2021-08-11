module Arkham.Types.Asset.Cards.ResearchLibrarian where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Restriction
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Trait

newtype ResearchLibrarian = ResearchLibrarian AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

researchLibrarian :: AssetCard ResearchLibrarian
researchLibrarian = ally ResearchLibrarian Cards.researchLibrarian (1, 1)

instance HasModifiersFor env ResearchLibrarian

instance HasActions ResearchLibrarian where
  getActions (ResearchLibrarian x) =
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
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (SearchDeckForTraits iid (InvestigatorTarget iid) [Tome])
    _ -> ResearchLibrarian <$> runMessage msg attrs
