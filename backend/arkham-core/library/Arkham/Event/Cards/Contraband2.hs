module Arkham.Event.Cards.Contraband2
  ( contraband2
  , Contraband2(..)
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

newtype Contraband2 = Contraband2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

contraband2 :: EventCard Contraband2
contraband2 = event Contraband2 Cards.contraband2

instance
  ( HasQueue env
  , HasId LocationId env InvestigatorId
  , HasSet InvestigatorId env LocationId
  , HasCount UsesCount env AssetId
  , Query AssetMatcher env
  )
  => RunMessage env Contraband2 where
  runMessage msg e@(Contraband2 attrs@EventAttrs {..}) = case msg of
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

      e <$ push
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
