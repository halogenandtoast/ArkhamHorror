module Arkham.Types.Asset.Cards.ResearchLibrarian where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Window

newtype ResearchLibrarian = ResearchLibrarian AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

researchLibrarian :: AssetCard ResearchLibrarian
researchLibrarian = ally ResearchLibrarian Cards.researchLibrarian (1, 1)

instance HasModifiersFor env ResearchLibrarian

instance HasAbilities env ResearchLibrarian where
  getAbilities i (WhenEnterPlay target) (ResearchLibrarian x)
    | isTarget x target && ownedBy x i = pure
      [mkAbility (toSource x) 1 (LegacyReactionAbility Free)]
  getAbilities i window (ResearchLibrarian x) = getAbilities i window x

instance (AssetRunner env) => RunMessage env ResearchLibrarian where
  runMessage msg a@(ResearchLibrarian attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (SearchDeckForTraits iid (InvestigatorTarget iid) [Tome])
    _ -> ResearchLibrarian <$> runMessage msg attrs
