module Arkham.Types.Asset.Cards.ArtStudent
  ( artStudent
  , ArtStudent(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Window

newtype ArtStudent = ArtStudent AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

artStudent :: AssetCard ArtStudent
artStudent = ally ArtStudent Cards.artStudent (1, 2)

instance HasActions env ArtStudent where
  getActions i (WhenEnterPlay target) (ArtStudent x) | isTarget x target =
    pure [UseAbility i (mkAbility (toSource x) 1 (ReactionAbility Free))]
  getActions iid window (ArtStudent attrs) = getActions iid window attrs

instance HasModifiersFor env ArtStudent

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env ArtStudent where
  runMessage msg a@(ArtStudent attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (InvestigatorDiscoverCluesAtTheirLocation iid 1 Nothing)
    _ -> ArtStudent <$> runMessage msg attrs
