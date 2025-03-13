module Arkham.Asset.Assets.SacredOathOathOfWisdom5 (sacredOathOathOfWisdom5) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Window (cardDrawn)
import Arkham.Matcher
import Arkham.Trait

newtype SacredOathOathOfWisdom5 = SacredOathOathOfWisdom5 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sacredOathOathOfWisdom5 :: AssetCard SacredOathOathOfWisdom5
sacredOathOathOfWisdom5 = asset SacredOathOathOfWisdom5 Cards.sacredOathOathOfWisdom5

instance HasModifiersFor SacredOathOathOfWisdom5 where
  getModifiersFor (SacredOathOathOfWisdom5 a) = modifySelf a [SharesSlotWith 3 "Sacred Oath"]

instance HasAbilities SacredOathOathOfWisdom5 where
  getAbilities (SacredOathOathOfWisdom5 a) =
    [ reactionAbility
        a
        1
        (exhaust a <> DiscardAssetCost (AssetControlledBy You <> "Sacred Oath"))
        ( DrawCard
            #when
            You
            (CanCancelRevelationEffect $ basic $ NonWeaknessTreachery <> hasAnyTrait [Omen, Power])
            AnyDeck
        )
        ControlsThis
    ]

instance RunMessage SacredOathOathOfWisdom5 where
  runMessage msg a@(SacredOathOathOfWisdom5 attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (cardDrawn -> card) _ -> do
      cancelRevelation attrs card
      cancelledOrIgnoredCardOrGameEffect (attrs.ability 1)
      pure a
    _ -> SacredOathOathOfWisdom5 <$> liftRunMessage msg attrs
