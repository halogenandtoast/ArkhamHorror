module Arkham.Types.Location.Cards.BurnedRuins_205
  ( burnedRuins_205
  , BurnedRuins_205(..)
  ) where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.Effect.Window
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.EncounterSet (EncounterSet)
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.Exception
import Arkham.Types.GameValue
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.ScenarioId
import Arkham.Types.SkillId
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Stats (Stats)
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.TreacheryId
import Arkham.Types.Window


import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype BurnedRuins_205 = BurnedRuins_205 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

burnedRuins_205 :: BurnedRuins_205
burnedRuins_205 = BurnedRuins_205 $ baseAttrs
  "02205"
  (Name "Burned Ruins" Nothing)
  EncounterSet.BloodOnTheAltar
  2
  (Static 3)
  Triangle
  [Square, Diamond]
  [Dunwich]

instance HasModifiersFor env BurnedRuins_205 where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env BurnedRuins_205 where
  getActions = withDrawCardUnderneathAction

instance LocationRunner env => RunMessage env BurnedRuins_205 where
  runMessage msg (BurnedRuins_205 attrs) = case msg of
    AfterFailedInvestigate _ target | isTarget attrs target -> do
      pure
        . BurnedRuins_205
        $ (if locationClues attrs > 0
            then attrs & cluesL -~ 1 & doomL +~ 1
            else attrs
          )
    _ -> BurnedRuins_205 <$> runMessage msg attrs
