module Arkham.Types.Asset.Cards.StrangeSolution
  ( strangeSolution
  , StrangeSolution(..)
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


import Arkham.Types.Asset.Attrs
import Arkham.Types.CampaignLogKey

newtype StrangeSolution = StrangeSolution AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangeSolution :: AssetId -> StrangeSolution
strangeSolution uuid = StrangeSolution $ baseAttrs uuid "02021"

instance HasActions env StrangeSolution where
  getActions iid NonFast (StrangeSolution attrs) | ownedBy attrs iid = pure
    [ ActivateCardAbilityAction
        iid
        (mkAbility (toSource attrs) 1 (ActionAbility Nothing $ ActionCost 1))
    ]
  getActions iid window (StrangeSolution attrs) = getActions iid window attrs

instance HasModifiersFor env StrangeSolution where
  getModifiersFor = noModifiersFor

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env StrangeSolution where
  runMessage msg a@(StrangeSolution attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessage
        (BeginSkillTest
          iid
          source
          (InvestigatorTarget iid)
          Nothing
          SkillIntellect
          4
        )
    PassedSkillTest iid _ source _ _ _ | isSource attrs source ->
      a <$ unshiftMessages
        [ Discard (toTarget attrs)
        , DrawCards iid 2 False
        , Record YouHaveIdentifiedTheSolution
        ]
    _ -> StrangeSolution <$> runMessage msg attrs
