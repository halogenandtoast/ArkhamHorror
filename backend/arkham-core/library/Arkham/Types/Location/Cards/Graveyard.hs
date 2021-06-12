module Arkham.Types.Location.Cards.Graveyard where

import Arkham.Prelude

import Arkham.Types.Classes
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Trait

newtype Graveyard = Graveyard LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

graveyard :: LocationId -> Graveyard
graveyard = Graveyard . (victoryL ?~ 1) . baseAttrs
  "01133"
  "Graveyard"
  EncounterSet.TheMidnightMasks
  1
  (PerPlayer 2)
  Hourglass
  [Circle]
  [Arkham]

instance HasModifiersFor env Graveyard where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env Graveyard where
  getActions i window (Graveyard attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env Graveyard where
  runMessage msg l@(Graveyard attrs@LocationAttrs {..}) = case msg of
    AfterEnterLocation iid lid | lid == locationId -> do
      unshiftMessage
        (BeginSkillTest
          iid
          (LocationSource lid)
          (InvestigatorTarget iid)
          Nothing
          SkillWillpower
          3
        )
      Graveyard <$> runMessage msg attrs
    FailedSkillTest iid _ source _ _ _ | isSource attrs source -> do
      rivertownId <- getJustLocationIdByName "Rivertown"
      l <$ unshiftMessage
        (chooseOne
          iid
          [ InvestigatorAssignDamage iid source DamageAny 0 2
          , MoveTo iid rivertownId
          ]
        )
    _ -> Graveyard <$> runMessage msg attrs
