module Arkham.Asset.Assets.Dalkura (dalkura) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype Dalkura = Dalkura AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dalkura :: AssetCard Dalkura
dalkura = ally Dalkura Cards.dalkura (2, 1)

instance HasAbilities Dalkura where
  getAbilities (Dalkura a) =
    [ restricted
        a
        1
        (ControlsThis <> ExtendedCardExists (InHandOf NotForPlay You <> CardWithDreaming AnyValue))
        $ parleyAction
        $ exhaust a
        <> ResourceCost 1
        <> UpTo (Fixed 4) (ResourceCost 1)
    ]

instance RunMessage Dalkura where
  runMessage msg a@(Dalkura attrs) = runQueueT $ case msg of
    -- X is read back off the payment: the 1 mandatory resource plus the 0-4 'UpTo' chose.
    UseCardAbility iid (isSource attrs -> True) 1 _ ((.resources) -> x) -> do
      cards <- select $ inHandOf NotForPlay iid <> CardWithDreaming (static x)
      -- The cost names X before X is known to match anything, so an unaffordable
      -- X is inherent and the criterion cannot predict it. Say so instead: an
      -- empty choice is skipped silently, spending the action for nothing (#6017).
      if null cards
        then push $ NoCardInHandWithDreaming iid x
        else chooseOrRunOneM iid $ targets cards \card -> do
          costModifier (attrs.ability 1) card IgnoreAllCosts
          playCardPayingCost iid card
      pure a
    _ -> Dalkura <$> liftRunMessage msg attrs
