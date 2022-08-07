module Arkham.Event.Cards.Contraband2
  ( contraband2
  , Contraband2(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Types ( Field (..) )
import Arkham.Asset.Uses
import Arkham.Classes
import Arkham.Event.Runner
import Arkham.Event.Cards qualified as Cards
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Target

newtype Contraband2 = Contraband2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

contraband2 :: EventCard Contraband2
contraband2 = event Contraband2 Cards.contraband2

instance RunMessage Contraband2 where
  runMessage msg e@(Contraband2 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      investigatorIds <-
        selectList
        $ InvestigatorAt
        $ LocationWithInvestigator
        $ InvestigatorWithId iid

      ammoAssets <- selectList
        (AssetWithUseType Ammo <> AssetOneOf
          (map (AssetControlledBy . InvestigatorWithId) investigatorIds)
        )

      ammoAssetsWithUseCount <- map (\(c, aid) -> (Ammo, c, aid)) <$> for
        ammoAssets
        (\aid -> (, aid) . useCount <$> field AssetUses aid)

      supplyAssets <- selectList
        (AssetWithUseType Supply <> AssetOneOf
          (map (AssetControlledBy . InvestigatorWithId) investigatorIds)
        )

      supplyAssetsWithUseCount <- map (\(c, aid) -> (Supply, c, aid)) <$> for
        supplyAssets
        (\aid -> (, aid) . useCount <$> field AssetUses aid)

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
