module Arkham.Types.Event.Cards.Contraband2
  ( contraband2
  , Contraband2(..)
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


import Arkham.Types.Asset.Uses
import Arkham.Types.Event.Attrs

newtype Contraband2 = Contraband2 EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

contraband2 :: InvestigatorId -> EventId -> Contraband2
contraband2 iid uuid = Contraband2 $ baseAttrs iid uuid "02109"

instance HasActions env Contraband2 where
  getActions iid window (Contraband2 attrs) = getActions iid window attrs

instance HasModifiersFor env Contraband2 where
  getModifiersFor = noModifiersFor

instance
  ( HasQueue env
  , HasId LocationId env InvestigatorId
  , HasSet InvestigatorId env LocationId
  , HasSet AssetId env (InvestigatorId, UseType)
  , HasCount UsesCount env AssetId
  )
  => RunMessage env Contraband2 where
  runMessage msg e@(Contraband2 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      locationId <- getId @LocationId iid
      investigatorIds <- getSetList @InvestigatorId locationId
      ammoAssets <- concat
        <$> for investigatorIds (getSetList @AssetId . (, Ammo))

      ammoAssetsWithUseCount <- map (\(c, aid) -> (Ammo, c, aid))
        <$> for ammoAssets (\aid -> (, aid) . unUsesCount <$> getCount aid)

      supplyAssets <- concat
        <$> for investigatorIds (getSetList @AssetId . (, Supply))

      supplyAssetsWithUseCount <- map (\(c, aid) -> (Supply, c, aid))
        <$> for supplyAssets (\aid -> (, aid) . unUsesCount <$> getCount aid)

      e <$ unshiftMessage
        (chooseOne
          iid
          [ Label
            "Place 2 ammo or supply tokens on that asset and draw 1 card."
            [ chooseOne
                iid
                [ TargetLabel
                    (AssetTarget assetId)
                    [ AddUses (AssetTarget assetId) useType' 2
                    , DrawCards iid 1 False
                    ]
                | (useType', _, assetId) <-
                  ammoAssetsWithUseCount <> supplyAssetsWithUseCount
                ]
            ]
          , Label
            "Double the number of ammo or supply tokens on that asset."
            [ chooseOne
                iid
                [ TargetLabel
                    (AssetTarget assetId)
                    [AddUses (AssetTarget assetId) useType' assetUseCount]
                | (useType', assetUseCount, assetId) <-
                  ammoAssetsWithUseCount <> supplyAssetsWithUseCount
                ]
            ]
          ]
        )
    _ -> Contraband2 <$> runMessage msg attrs
