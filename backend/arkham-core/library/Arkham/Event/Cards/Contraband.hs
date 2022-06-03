module Arkham.Event.Cards.Contraband
  ( contraband
  , Contraband(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Asset.Uses
import Arkham.Classes
import Arkham.Event.Attrs
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Query
import Arkham.Target

newtype Contraband = Contraband EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

contraband :: EventCard Contraband
contraband = event Contraband Cards.contraband

instance
  ( HasQueue env
  , Query AssetMatcher env
  , HasId LocationId env InvestigatorId
  , HasSet InvestigatorId env LocationId
  , HasCount UsesCount env AssetId
  )
  => RunMessage Contraband where
  runMessage msg e@(Contraband attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      locationId <- getId @LocationId iid
      investigatorIds <- getSetList @InvestigatorId locationId

      ammoAssets <- selectList
        (AssetWithUseType Ammo <> AssetOneOf
          (map (AssetControlledBy . InvestigatorWithId) investigatorIds)
        )

      ammoAssetsWithUseCount <- map (\(c, aid) -> (Ammo, c, aid))
        <$> for ammoAssets (\aid -> (, aid) . unUsesCount <$> getCount aid)

      supplyAssets <- selectList
        (AssetWithUseType Supply <> AssetOneOf
          (map (AssetControlledBy . InvestigatorWithId) investigatorIds)
        )

      supplyAssetsWithUseCount <- map (\(c, aid) -> (Supply, c, aid))
        <$> for supplyAssets (\aid -> (, aid) . unUsesCount <$> getCount aid)

      e <$ pushAll
        [ chooseOne
          iid
          [ TargetLabel
              (AssetTarget assetId)
              [AddUses (AssetTarget assetId) useType' assetUseCount]
          | (useType', assetUseCount, assetId) <-
            ammoAssetsWithUseCount <> supplyAssetsWithUseCount
          ]
        , Discard (toTarget attrs)
        ]
    _ -> Contraband <$> runMessage msg attrs
