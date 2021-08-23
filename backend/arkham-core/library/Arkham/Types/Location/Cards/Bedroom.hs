module Arkham.Types.Location.Cards.Bedroom where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (bedroom)
import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Message

newtype Bedroom = Bedroom LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bedroom :: LocationCard Bedroom
bedroom = location Bedroom Cards.bedroom 2 (PerPlayer 1) Heart [T]

instance HasModifiersFor env Bedroom

instance HasAbilities env Bedroom where
  getAbilities i window (Bedroom attrs) = getAbilities i window attrs

instance (LocationRunner env) => RunMessage env Bedroom where
  runMessage msg l@(Bedroom attrs) = case msg of
    After (FailedSkillTest iid (Just Action.Investigate) _ target _ _)
      | isTarget attrs target -> l <$ push (RandomDiscard iid)
    _ -> Bedroom <$> runMessage msg attrs
