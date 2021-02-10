module Arkham.Types.Location.Cards.AdministrationOffice_131
  ( administrationOffice_131
  , AdministrationOffice_131(..)
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
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype AdministrationOffice_131 = AdministrationOffice_131 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

administrationOffice_131 :: AdministrationOffice_131
administrationOffice_131 = AdministrationOffice_131 $ baseAttrs
  "02131"
  (Name "Administration Office" Nothing)
  EncounterSet.TheMiskatonicMuseum
  2
  (PerPlayer 2)
  Triangle
  [Square]
  (singleton Miskatonic)

instance HasCount CardCount env InvestigatorId => HasModifiersFor env AdministrationOffice_131 where
  getModifiersFor (InvestigatorSource iid) target (AdministrationOffice_131 attrs)
    | isTarget attrs target
    = do
      cardsInHand <- unCardCount <$> getCount iid
      pure $ toModifiers attrs [ CannotInvestigate | cardsInHand <= 4 ]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env AdministrationOffice_131 where
  getActions iid window (AdministrationOffice_131 attrs) =
    getActions iid window attrs

instance LocationRunner env => RunMessage env AdministrationOffice_131 where
  runMessage msg (AdministrationOffice_131 attrs) =
    AdministrationOffice_131 <$> runMessage msg attrs
