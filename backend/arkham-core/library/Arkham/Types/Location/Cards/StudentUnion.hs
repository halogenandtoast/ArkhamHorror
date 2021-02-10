module Arkham.Types.Location.Cards.StudentUnion
  ( StudentUnion(..)
  , studentUnion
  )
where

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

newtype StudentUnion = StudentUnion LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

studentUnion :: StudentUnion
studentUnion = StudentUnion $ baseAttrs
  "02051"
  (Name "Student Union" Nothing)
  EncounterSet.ExtracurricularActivity
  1
  (Static 2)
  Diamond
  [Plus, Equals]
  [Miskatonic]

instance HasModifiersFor env StudentUnion where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env StudentUnion where
  getActions iid NonFast (StudentUnion attrs@LocationAttrs {..}) | locationRevealed =
    withBaseActions iid NonFast attrs $ do
      let
        ability =
          mkAbility (toSource attrs) 1 (ActionAbility Nothing $ ActionCost 2)
      pure
        [ ActivateCardAbilityAction iid ability
        | iid `elem` locationInvestigators
        ]
  getActions iid window (StudentUnion attrs) = getActions iid window attrs

instance (LocationRunner env) => RunMessage env StudentUnion where
  runMessage msg l@(StudentUnion attrs) = case msg of
    RevealLocation _ lid | lid == locationId attrs -> do
      unshiftMessage $ PlaceLocationMatching (LocationWithTitle "Dormitories")
      StudentUnion <$> runMessage msg attrs
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ unshiftMessages
        [ HealDamage (InvestigatorTarget iid) 1
        , HealHorror (InvestigatorTarget iid) 1
        ]
    _ -> StudentUnion <$> runMessage msg attrs
