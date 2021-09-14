module Arkham.Types.Location.Cards.MainPath
  ( MainPath(..)
  , mainPath
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (mainPath)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Trait

newtype MainPath = MainPath LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mainPath :: LocationCard MainPath
mainPath = locationWith
  MainPath
  Cards.mainPath
  2
  (Static 0)
  Squiggle
  [Square, Plus]
  (revealedConnectedMatchersL <>~ [LocationWithTrait Woods])

instance HasAbilities MainPath where
  getAbilities (MainPath a) = withResignAction a []

instance LocationRunner env => RunMessage env MainPath where
  runMessage msg (MainPath attrs) = MainPath <$> runMessage msg attrs
