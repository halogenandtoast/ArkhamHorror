module Arkham.Asset.Assets.TheGrapevine (theGrapevine) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Location
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype TheGrapevine = TheGrapevine AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGrapevine :: AssetCard TheGrapevine
theGrapevine = assetWith TheGrapevine Cards.theGrapevine discardWhenNoUses

instance HasAbilities TheGrapevine where
  getAbilities (TheGrapevine a) =
    [ controlled
        a
        1
        ( exists
            ( oneOf
                [ EnemyAt (LocationWithDistanceFromAtMost 2 YourLocation (not_ YourLocation <> RevealedLocation))
                , EnemyAt YourLocation <> not_ EnemyEngagedWithYou <> CanEngageEnemy (a.ability 1)
                ]
            )
        )
        $ parleyAction
        $ assetUseCost a Rumor 1
        <> exhaust a
    ]

instance RunMessage TheGrapevine where
  runMessage msg a@(TheGrapevine attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <-
        select
          $ oneOf
            [ EnemyAt
                ( LocationWithDistanceFromAtMost
                    2
                    (locationWithInvestigator iid)
                    (not_ YourLocation <> RevealedLocation)
                )
            , EnemyAt YourLocation <> not_ EnemyEngagedWithYou <> CanEngageEnemy (attrs.ability 1)
            ]

      chooseTargetM iid enemies \eid -> do
        withLocationOf eid $ moveUntil iid
        engageEnemy iid eid
      pure a
    _ -> TheGrapevine <$> liftRunMessage msg attrs
