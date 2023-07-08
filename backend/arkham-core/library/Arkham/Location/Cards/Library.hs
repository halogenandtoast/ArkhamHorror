module Arkham.Location.Cards.Library (
  library,
  Library (..),
)
where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher

newtype Library = Library LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

library :: LocationCard Library
library = location Library Cards.library 6 (PerPlayer 1)

instance HasModifiersFor Library where
  getModifiersFor (LocationTarget lid) (Library attrs) | lid == toId attrs = do
    mskillTestSource <- getSkillTestSource
    case mskillTestSource of
      Just (SkillTestSource iid _ source (Just Action.Investigate))
        | isSource attrs source -> do
            hasTabletKey <- iid <=~> InvestigatorWithKey TabletKey
            pure $
              toModifiers
                attrs
                [ShroudModifier (-3) | locationRevealed attrs && hasTabletKey]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage Library where
  runMessage msg (Library attrs) =
    Library <$> runMessage msg attrs
