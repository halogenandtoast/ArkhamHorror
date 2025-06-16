module Arkham.Asset.Assets.ArseneRenard (
  arseneRenard,
  ArseneRenard(..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher

newtype ArseneRenard = ArseneRenard AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arseneRenard :: AssetCard ArseneRenard
arseneRenard = allyWith ArseneRenard Cards.arseneRenard (1, 3) noSlots

instance HasAbilities ArseneRenard where
  getAbilities (ArseneRenard a) =
    [ limitedAbility (GroupLimit PerGame 1)
        $ controlledAbility a 1 ControlsThis
        $ FastAbility Free
    , controlledAbility
        a
        2
        ( TokensOnLocation YouLocation Token.Antiquity (atLeast 1)
            <> exists (EnemyAt YourLocation <> EnemyExhausted)
        )
        $ FastAbility Free
    ]

instance RunMessage ArseneRenard where
  runMessage msg a@(ArseneRenard attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      locations <- select AnyLocation
      for_ locations \lid -> placeTokens (attrs.ability 1) lid Token.Antiquity 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withLocationOf iid \lid -> removeTokens (attrs.ability 2) lid Token.Antiquity 1
      player <- getPlayer iid
      chooseOrRunOne player
        [ Label "Draw 1 card" [drawCards iid (attrs.ability 2) 1]
        , Label "Gain 2 resources" [gainResources iid (attrs.ability 2) 2]
        ]
      pure a
    _ -> ArseneRenard <$> liftRunMessage msg attrs
