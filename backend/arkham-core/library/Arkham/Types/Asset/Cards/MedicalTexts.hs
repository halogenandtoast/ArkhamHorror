module Arkham.Types.Asset.Cards.MedicalTexts
  ( MedicalTexts(..)
  , medicalTexts
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
import Arkham.Types.Asset.Runner

newtype MedicalTexts = MedicalTexts AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

medicalTexts :: AssetId -> MedicalTexts
medicalTexts uuid =
  MedicalTexts $ (baseAttrs uuid "01035") { assetSlots = [HandSlot] }

instance HasModifiersFor env MedicalTexts where
  getModifiersFor = noModifiersFor

instance HasActions env MedicalTexts where
  getActions iid NonFast (MedicalTexts a) | ownedBy a iid = pure
    [ ActivateCardAbilityAction
        iid
        (mkAbility (toSource a) 1 (ActionAbility Nothing $ ActionCost 1))
    ]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env MedicalTexts where
  runMessage msg a@(MedicalTexts attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      locationId <- getId @LocationId (getInvestigator attrs)
      locationInvestigatorIds <- getSetList locationId
      unshiftMessage
        (chooseOne
          iid
          [ BeginSkillTest
              iid
              source
              (InvestigatorTarget iid')
              Nothing
              SkillIntellect
              2
          | iid' <- locationInvestigatorIds
          ]
        )
      MedicalTexts <$> runMessage msg attrs
    PassedSkillTest _ _ source target _ _ | isSource attrs source ->
      a <$ unshiftMessage (HealDamage target 1)
    FailedSkillTest _ _ source (InvestigatorTarget iid) _ _
      | isSource attrs source -> a
      <$ unshiftMessage (InvestigatorAssignDamage iid source DamageAny 1 0)
    _ -> MedicalTexts <$> runMessage msg attrs
