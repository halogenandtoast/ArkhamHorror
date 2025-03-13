module Arkham.Asset.Assets.SacredOathOathOfLoyalty5 (sacredOathOathOfLoyalty5) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Window (cardDrawn)
import Arkham.Matcher
import Arkham.Trait

newtype SacredOathOathOfLoyalty5 = SacredOathOathOfLoyalty5 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sacredOathOathOfLoyalty5 :: AssetCard SacredOathOathOfLoyalty5
sacredOathOathOfLoyalty5 = asset SacredOathOathOfLoyalty5 Cards.sacredOathOathOfLoyalty5

instance HasModifiersFor SacredOathOathOfLoyalty5 where
  getModifiersFor (SacredOathOathOfLoyalty5 a) = modifySelf a [SharesSlotWith 3 "Sacred Oath"]

instance HasAbilities SacredOathOathOfLoyalty5 where
  getAbilities (SacredOathOathOfLoyalty5 a) =
    [ reactionAbility
        a
        1
        (exhaust a <> DiscardAssetCost (AssetControlledBy You <> "Sacred Oath"))
        ( DrawCard
            #when
            You
            (CanCancelRevelationEffect $ basic $ NonWeaknessTreachery <> hasAnyTrait [Pact, Terror])
            AnyDeck
        )
        ControlsThis
    ]

instance RunMessage SacredOathOathOfLoyalty5 where
  runMessage msg a@(SacredOathOathOfLoyalty5 attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (cardDrawn -> card) _ -> do
      cancelRevelation attrs card
      cancelledOrIgnoredCardOrGameEffect (attrs.ability 1)
      pure a
    _ -> SacredOathOathOfLoyalty5 <$> liftRunMessage msg attrs
