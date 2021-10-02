module Arkham.Types.Event.Cards.Contraband
  ( contraband
  , Contraband(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Types.Asset.Uses
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Target

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
  => RunMessage env Contraband where
  runMessage msg e@(Contraband attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      locationId <- getId @LocationId iid
      investigatorIds <- getSetList @InvestigatorId locationId

      ammoAssets <- selectList
        (AssetWithUseType Ammo <> AssetOneOf
          (map (AssetOwnedBy . InvestigatorWithId) investigatorIds)
        )

      ammoAssetsWithUseCount <- map (\(c, aid) -> (Ammo, c, aid))
        <$> for ammoAssets (\aid -> (, aid) . unUsesCount <$> getCount aid)

      supplyAssets <- selectList
        (AssetWithUseType Supply <> AssetOneOf
          (map (AssetOwnedBy . InvestigatorWithId) investigatorIds)
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
