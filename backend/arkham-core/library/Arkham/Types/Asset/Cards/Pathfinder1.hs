module Arkham.Types.Asset.Cards.Pathfinder1
  ( pathfinder1
  , Pathfinder1(..)
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


import Arkham.Types.Asset.Attrs

newtype Pathfinder1 = Pathfinder1 AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pathfinder1 :: AssetId -> Pathfinder1
pathfinder1 uuid = Pathfinder1 $ baseAttrs uuid "02108"

ability :: AssetAttrs -> Ability
ability attrs =
  mkAbility (toSource attrs) 1 (FastAbility $ ExhaustCost (toTarget attrs))

instance HasSet EnemyId env InvestigatorId => HasActions env Pathfinder1 where
  getActions iid (DuringTurn You) (Pathfinder1 attrs) = do
    engagedEnemies <- getSet @EnemyId iid
    pure [ ActivateCardAbilityAction iid (ability attrs) | null engagedEnemies ]
  getActions _ _ _ = pure []

instance HasModifiersFor env Pathfinder1 where
  getModifiersFor = noModifiersFor

instance
  ( HasQueue env
  , HasModifiersFor env ()
  , HasSet AccessibleLocationId env LocationId
  , HasId LocationId env InvestigatorId
  )
  => RunMessage env Pathfinder1 where
  runMessage msg a@(Pathfinder1 attrs@AssetAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      accessibleLocationIds <-
        map unAccessibleLocationId <$> (getSetList =<< getId @LocationId iid)
      a <$ unshiftMessage
        (chooseOne
          iid
          [ MoveAction iid lid False | lid <- accessibleLocationIds ]
        )
    _ -> Pathfinder1 <$> runMessage msg attrs
