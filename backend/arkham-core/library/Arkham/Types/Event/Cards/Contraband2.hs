module Arkham.Types.Event.Cards.Contraband2
  ( contraband2
  , Contraband2(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Asset.Uses
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Target

newtype Contraband2 = Contraband2 EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

contraband2 :: EventCard Contraband2
contraband2 = event Contraband2 Cards.contraband2

instance HasActions Contraband2
instance HasModifiersFor env Contraband2

instance
  ( HasQueue env
  , HasId LocationId env InvestigatorId
  , HasSet InvestigatorId env LocationId
  , HasCount UsesCount env AssetId
  , Query AssetMatcher env
  )
  => RunMessage env Contraband2 where
  runMessage msg e@(Contraband2 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == eventId -> do
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
