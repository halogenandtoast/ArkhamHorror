module Arkham.Asset.Assets.TheGrapevine2 (theGrapevine2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Capability
import Arkham.Helpers.Location
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype TheGrapevine2 = TheGrapevine2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGrapevine2 :: AssetCard TheGrapevine2
theGrapevine2 = assetWith TheGrapevine2 Cards.theGrapevine2 discardWhenNoUses

instance HasAbilities TheGrapevine2 where
  getAbilities (TheGrapevine2 a) =
    [ controlled
        a
        1
        ( oneOf
            [ exists
                ( oneOf
                    [ EnemyAt (LocationWithDistanceFromAtMost 3 YourLocation (not_ YourLocation <> RevealedLocation))
                    , EnemyAt YourLocation <> not_ EnemyEngagedWithYou <> CanEngageEnemy (a.ability 1)
                    ]
                )
            , youExist can.draw.cards
            ]
        )
        $ parleyAction
        $ assetUseCost a Rumor 1
        <> exhaust a
    ]

instance RunMessage TheGrapevine2 where
  runMessage msg a@(TheGrapevine2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <-
        select
          $ oneOf
            [ EnemyAt
                ( LocationWithDistanceFromAtMost
                    3
                    (locationWithInvestigator iid)
                    (not_ YourLocation <> RevealedLocation)
                )
            , EnemyAt YourLocation <> not_ EnemyEngagedWithYou <> CanEngageEnemy (attrs.ability 1)
            ]
      chooseTargetM iid enemies \eid -> do
        withLocationOf eid $ moveUntil iid
        engageEnemy iid eid
      drawCardsIfCan iid (attrs.ability 1) 1
      pure a
    _ -> TheGrapevine2 <$> liftRunMessage msg attrs
