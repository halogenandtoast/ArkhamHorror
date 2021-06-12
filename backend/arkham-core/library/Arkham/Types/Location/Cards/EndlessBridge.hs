module Arkham.Types.Location.Cards.EndlessBridge
  ( endlessBridge
  , EndlessBridge(..)
  )
where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.Classes
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Trait
import Arkham.Types.Window

newtype EndlessBridge = EndlessBridge LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

endlessBridge :: LocationId -> EndlessBridge
endlessBridge = EndlessBridge . baseAttrs
  "02326"
  "Endless Bridge"
  EncounterSet.LostInTimeAndSpace
  4
  (Static 2)
  Triangle
  [Square, Squiggle]
  [Otherworld, Extradimensional]

instance HasModifiersFor env EndlessBridge where
  getModifiersFor = noModifiersFor

forcedAbility :: LocationAttrs -> Ability
forcedAbility a = mkAbility (toSource a) 1 ForcedAbility

instance ActionRunner env => HasActions env EndlessBridge where
  getActions _ (AfterLeaving You lid) (EndlessBridge attrs)
    | lid == toId attrs = do
      leadInvestigator <- getLeadInvestigatorId
      pure [ActivateCardAbilityAction leadInvestigator (forcedAbility attrs)]
  getActions iid window (EndlessBridge attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env EndlessBridge where
  runMessage msg l@(EndlessBridge attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      unshiftMessages
        [ PlaceLocation (locationCardCode attrs) (toId attrs)
        , LoseResources iid 2
        ]
      EndlessBridge <$> runMessage msg attrs
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ unshiftMessage
        (chooseOne
          iid
          [ Label
            "Place 1 doom on Endless Bridge"
            [PlaceDoom (toTarget attrs) 1]
          , Label "Discard Endless Bridge" [Discard (toTarget attrs)]
          ]
        )
    _ -> EndlessBridge <$> runMessage msg attrs
