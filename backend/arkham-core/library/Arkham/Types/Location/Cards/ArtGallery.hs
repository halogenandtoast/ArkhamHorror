module Arkham.Types.Location.Cards.ArtGallery
  ( artGallery
  , ArtGallery(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (artGallery)
import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Message
import Arkham.Types.Target

newtype ArtGallery = ArtGallery LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

artGallery :: LocationCard ArtGallery
artGallery = locationWith
  ArtGallery
  Cards.artGallery
  2
  (PerPlayer 1)
  T
  [Diamond]
  (revealedSymbolL .~ Hourglass)

instance HasModifiersFor env ArtGallery

instance HasAbilities env ArtGallery where
  getAbilities iid window (ArtGallery attrs) = getAbilities iid window attrs

instance LocationRunner env => RunMessage env ArtGallery where
  runMessage msg l@(ArtGallery attrs) = case msg of
    After (FailedSkillTest iid (Just Action.Investigate) _ (SkillTestInitiatorTarget _) _ _)
      -> l <$ push (SpendResources iid 2)
    _ -> ArtGallery <$> runMessage msg attrs
