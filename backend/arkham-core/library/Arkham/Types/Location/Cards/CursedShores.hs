module Arkham.Types.Location.Cards.CursedShores
  ( CursedShores(..)
  , cursedShores
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

newtype CursedShores = CursedShores LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cursedShores :: CursedShores
cursedShores = CursedShores $ baseAttrs
  "81007"
  (Name "Cursed Shores" Nothing)
  EncounterSet.CurseOfTheRougarou
  1
  (Static 0)
  Square
  [Plus, Triangle, Diamond, Hourglass]
  [NewOrleans, Bayou]

instance HasModifiersFor env CursedShores where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env CursedShores where
  getActions iid NonFast (CursedShores attrs@LocationAttrs {..}) | locationRevealed =
    withBaseActions iid NonFast attrs $ pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility (toSource attrs) 1 (ActionAbility Nothing $ ActionCost 1))
      | iid `member` locationInvestigators
      ]
  getActions i window (CursedShores attrs) = getActions i window attrs

instance LocationRunner env => RunMessage env CursedShores where
  runMessage msg l@(CursedShores attrs@LocationAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ unshiftMessages
      [ InvestigatorAssignDamage iid source DamageAny 1 0
      , CreateEffect "81007" Nothing (toSource attrs) (InvestigatorTarget iid)
      ]
    WhenEnterLocation iid lid
      | -- TODO: SHOULD WE BROADCAST LRAVING THE LOCATION INSTEAD
        lid /= locationId && iid `elem` locationInvestigators -> do
        skillCards <- map unHandCardId <$> getSetList (iid, SkillType)
        case skillCards of
          [] -> pure ()
          [x] -> unshiftMessage (DiscardCard iid x)
          xs -> unshiftMessage (chooseOne iid [ DiscardCard iid x | x <- xs ])
        CursedShores <$> runMessage msg attrs
    _ -> CursedShores <$> runMessage msg attrs
