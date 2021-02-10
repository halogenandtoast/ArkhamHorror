module Arkham.Types.Treachery.Cards.CryptChill where

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


import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype CryptChill = CryptChill TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cryptChill :: TreacheryId -> a -> CryptChill
cryptChill uuid _ = CryptChill $ baseAttrs uuid "01167"

instance HasModifiersFor env CryptChill where
  getModifiersFor = noModifiersFor

instance HasActions env CryptChill where
  getActions i window (CryptChill attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env CryptChill where
  runMessage msg t@(CryptChill attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> t <$ unshiftMessages
      [ RevelationSkillTest iid source SkillWillpower 4
      , Discard (TreacheryTarget treacheryId)
      ]
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        assetCount <- length <$> getSet @DiscardableAssetId iid
        if assetCount > 0
          then t <$ unshiftMessage (ChooseAndDiscardAsset iid)
          else
            t <$ unshiftMessage
              (InvestigatorAssignDamage iid source DamageAny 2 0)
    _ -> CryptChill <$> runMessage msg attrs
