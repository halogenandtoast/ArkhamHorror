module Arkham.Types.Location.Cards.PrismaticCascade
  ( prismaticCascade
  , PrismaticCascade(..)
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

newtype PrismaticCascade = PrismaticCascade LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

prismaticCascade :: LocationId -> PrismaticCascade
prismaticCascade = PrismaticCascade . baseAttrs
  "02325"
  "Prismatic Cascade"
  EncounterSet.LostInTimeAndSpace
  2
  (Static 3)
  Diamond
  [Square, Plus]
  [Otherworld, Extradimensional]

instance HasModifiersFor env PrismaticCascade where
  getModifiersFor = noModifiersFor

forcedAbility :: LocationAttrs -> Ability
forcedAbility a = mkAbility (toSource a) 1 ForcedAbility

instance ActionRunner env => HasActions env PrismaticCascade where
  getActions iid (AfterDiscoveringClues You YourLocation) (PrismaticCascade attrs)
    | iid `on` attrs
    = do
      leadInvestigator <- getLeadInvestigatorId
      pure
        [ ActivateCardAbilityAction leadInvestigator (forcedAbility attrs)
        | locationClues attrs == 0
        ]
  getActions iid window (PrismaticCascade attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env PrismaticCascade where
  runMessage msg l@(PrismaticCascade attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      unshiftMessage $ RandomDiscard iid
      PrismaticCascade <$> runMessage msg attrs
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      l <$ unshiftMessage (Discard $ toTarget attrs)
    _ -> PrismaticCascade <$> runMessage msg attrs
