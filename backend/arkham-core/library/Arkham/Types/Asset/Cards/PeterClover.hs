module Arkham.Types.Asset.Cards.PeterClover
  ( peterClover
  , PeterClover(..)
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
import Arkham.Types.Trait

newtype PeterClover = PeterClover AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

peterClover :: AssetId -> PeterClover
peterClover uuid = PeterClover
  $ (baseAttrs uuid "02079") { assetHealth = Just 3, assetSanity = Just 2 }

ability :: AssetAttrs -> Ability
ability attrs =
  mkAbility (toSource attrs) 1 (FastAbility $ ExhaustCost (toTarget attrs))

instance
  ( HasSet EnemyId env ([Trait], LocationId)
  , HasId LocationId env InvestigatorId
  )
  => HasActions env PeterClover where
  getActions iid FastPlayerWindow (PeterClover attrs) = do
    lid <- getId @LocationId iid
    criminals <- getSet @EnemyId ([Criminal], lid)
    pure
      [ ActivateCardAbilityAction iid (ability attrs) | not (null criminals) ]
  getActions iid window (PeterClover attrs) = getActions iid window attrs

instance HasModifiersFor env PeterClover where
  getModifiersFor = noModifiersFor

instance
  ( HasQueue env
  , HasModifiersFor env ()
  , HasSet EnemyId env ([Trait], LocationId)
  , HasId LocationId env InvestigatorId
  )
  => RunMessage env PeterClover where
  runMessage msg a@(PeterClover attrs@AssetAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      lid <- getId @LocationId iid
      criminals <- getSetList ([Criminal], lid)
      a <$ unshiftMessage
        (chooseOne iid [ EnemyEvaded iid eid | eid <- criminals ])
    BeginEnemy | isNothing assetInvestigator ->
      a <$ unshiftMessage (AssetDamage assetId (toSource attrs) 1 0)
    _ -> PeterClover <$> runMessage msg attrs
