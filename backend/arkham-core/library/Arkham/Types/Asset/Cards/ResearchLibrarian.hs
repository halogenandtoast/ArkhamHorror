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
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

researchLibrarian :: AssetCard ResearchLibrarian
researchLibrarian = ally ResearchLibrarian Cards.researchLibrarian (1, 1)

instance HasModifiersFor env ResearchLibrarian where
  getModifiersFor = noModifiersFor

instance HasActions env ResearchLibrarian where
  getActions i (WhenEnterPlay target) (ResearchLibrarian x)
    | isTarget x target = pure
      [ ActivateCardAbilityAction
          i
          (mkAbility (toSource x) 1 (ReactionAbility Free))
      ]
  getActions i window (ResearchLibrarian x) = getActions i window x

instance (AssetRunner env) => RunMessage env ResearchLibrarian where
  runMessage msg a@(ResearchLibrarian attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessage
        (SearchDeckForTraits iid (InvestigatorTarget iid) [Tome])
    _ -> ResearchLibrarian <$> runMessage msg attrs
