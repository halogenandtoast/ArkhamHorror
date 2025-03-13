module Arkham.Asset.Assets.SacredOathOathOfOrder5 (sacredOathOathOfOrder5) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Window (cardDrawn)
import Arkham.Matcher
import Arkham.Trait

newtype SacredOathOathOfOrder5 = SacredOathOathOfOrder5 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sacredOathOathOfOrder5 :: AssetCard SacredOathOathOfOrder5
sacredOathOathOfOrder5 = asset SacredOathOathOfOrder5 Cards.sacredOathOathOfOrder5

instance HasModifiersFor SacredOathOathOfOrder5 where
  getModifiersFor (SacredOathOathOfOrder5 a) = modifySelf a [SharesSlotWith 3 "Sacred Oath"]

instance HasAbilities SacredOathOathOfOrder5 where
  getAbilities (SacredOathOathOfOrder5 a) =
    [ reactionAbility
        a
        1
        (exhaust a <> DiscardAssetCost (AssetControlledBy You <> "Sacred Oath"))
        ( DrawCard
            #when
            You
            (CanCancelRevelationEffect $ basic $ NonWeaknessTreachery <> hasAnyTrait [Curse, Hex])
            AnyDeck
        )
        ControlsThis
    ]

instance RunMessage SacredOathOathOfOrder5 where
  runMessage msg a@(SacredOathOathOfOrder5 attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (cardDrawn -> card) _ -> do
      cancelRevelation attrs card
      cancelledOrIgnoredCardOrGameEffect (attrs.ability 1)
      pure a
    _ -> SacredOathOathOfOrder5 <$> liftRunMessage msg attrs
