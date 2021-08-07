module Arkham.Types.Location.Cards.TearThroughSpace
  ( tearThroughSpace
  , TearThroughSpace(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (tearThroughSpace)
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

newtype TearThroughSpace = TearThroughSpace LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tearThroughSpace :: LocationCard TearThroughSpace
tearThroughSpace = location
  TearThroughSpace
  Cards.tearThroughSpace
  1
  (Static 1)
  Square
  [Diamond, Triangle, Square]

instance HasModifiersFor env TearThroughSpace

forcedAbility :: LocationAttrs -> Ability
forcedAbility a = mkAbility (toSource a) 1 ForcedAbility

instance ActionRunner env => HasActions env TearThroughSpace where
  getActions iid AtEndOfRound (TearThroughSpace attrs) = do
    leadInvestigator <- getLeadInvestigatorId
    pure [ forcedAbility attrs | iid == leadInvestigator ]
  getActions iid window (TearThroughSpace attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env TearThroughSpace where
  runMessage msg l@(TearThroughSpace attrs) = case msg of
    Revelation _ source | isSource attrs source -> do
      let
        labels = [ nameToLabel (toName attrs) <> tshow @Int n | n <- [1 .. 4] ]
      availableLabel <- findM
        (fmap isNothing . getId @(Maybe LocationId) . LocationWithLabel)
        labels
      case availableLabel of
        Just label -> pure . TearThroughSpace $ attrs & labelL .~ label
        Nothing -> error "could not find label"
    UseCardAbility iid source _ 1 _ | isSource attrs source -> l <$ push
      (chooseOne
        iid
        [ Label
          "Place 1 doom on Tear through Space"
          [PlaceDoom (toTarget attrs) 1]
        , Label "Discard Tear through Space" [Discard (toTarget attrs)]
        ]
      )
    _ -> TearThroughSpace <$> runMessage msg attrs
