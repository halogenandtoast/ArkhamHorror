module Arkham.Types.Location.Cards.TearThroughSpace
  ( tearThroughSpace
  , TearThroughSpace(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (tearThroughSpace)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Window

newtype TearThroughSpace = TearThroughSpace LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tearThroughSpace :: LocationId -> TearThroughSpace
tearThroughSpace = TearThroughSpace . baseAttrs
  Cards.tearThroughSpace
  1
  (Static 1)
  Square
  [Diamond, Triangle, Square]

instance HasModifiersFor env TearThroughSpace where
  getModifiersFor = noModifiersFor

forcedAbility :: LocationAttrs -> Ability
forcedAbility a = mkAbility (toSource a) 1 ForcedAbility

instance ActionRunner env => HasActions env TearThroughSpace where
  getActions iid AtEndOfRound (TearThroughSpace attrs) = do
    leadInvestigator <- getLeadInvestigatorId
    pure
      [ ActivateCardAbilityAction leadInvestigator (forcedAbility attrs)
      | iid == leadInvestigator
      ]
  getActions iid window (TearThroughSpace attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env TearThroughSpace where
  runMessage msg l@(TearThroughSpace attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ unshiftMessage
        (chooseOne
          iid
          [ Label
            "Place 1 doom on Tear through Space"
            [PlaceDoom (toTarget attrs) 1]
          , Label "Discard Tear through Space" [Discard (toTarget attrs)]
          ]
        )
    _ -> TearThroughSpace <$> runMessage msg attrs
