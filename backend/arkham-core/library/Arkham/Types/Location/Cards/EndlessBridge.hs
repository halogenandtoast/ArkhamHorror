module Arkham.Types.Location.Cards.EndlessBridge
  ( endlessBridge
  , EndlessBridge(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (endlessBridge)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Name
import Arkham.Types.Window
import Control.Monad.Extra (findM)

newtype EndlessBridge = EndlessBridge LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

endlessBridge :: LocationCard EndlessBridge
endlessBridge = location
  EndlessBridge
  Cards.endlessBridge
  4
  (Static 2)
  Triangle
  [Square, Squiggle]

instance HasModifiersFor env EndlessBridge

forcedAbility :: LocationAttrs -> Ability
forcedAbility a = mkAbility (toSource a) 1 ForcedAbility

instance ActionRunner env => HasActions env EndlessBridge where
  getActions iid (AfterLeaving who lid) (EndlessBridge attrs)
    | lid == toId attrs && who == iid = do
      leadInvestigator <- getLeadInvestigatorId
      pure [ forcedAbility attrs | iid == leadInvestigator ]
  getActions iid window (EndlessBridge attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env EndlessBridge where
  runMessage msg l@(EndlessBridge attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      push $ LoseResources iid 2
      let
        labels = [ nameToLabel (toName attrs) <> tshow @Int n | n <- [1 .. 2] ]
      availableLabel <- findM
        (fmap isNothing . getId @(Maybe LocationId) . LocationWithLabel)
        labels
      case availableLabel of
        Just label -> pure . EndlessBridge $ attrs & labelL .~ label
        Nothing -> error "could not find label"
    UseCardAbility iid source _ 1 _ | isSource attrs source -> l <$ push
      (chooseOne
        iid
        [ Label "Place 1 doom on Endless Bridge" [PlaceDoom (toTarget attrs) 1]
        , Label "Discard Endless Bridge" [Discard (toTarget attrs)]
        ]
      )
    _ -> EndlessBridge <$> runMessage msg attrs
