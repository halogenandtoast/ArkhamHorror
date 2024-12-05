module Arkham.Asset.Assets.TheFool03 (theFool03, TheFool03 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGetsWhen)
import Arkham.Helpers.Window (cardPlayed)
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher

newtype TheFool03 = TheFool03 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theFool03 :: AssetCard TheFool03
theFool03 = asset TheFool03 Cards.theFool03

instance HasModifiersFor TheFool03 where
  getModifiersFor (TheFool03 a) = controllerGetsWhen a a.ready [CanReduceCostOf AnyCard 1]

instance HasAbilities TheFool03 where
  getAbilities (TheFool03 a) =
    [ restricted a 1 ControlsThis $ ReactionAbility (Matcher.PlayCard #when You #any) (exhaust a)
    , restricted a 2 InYourHand $ freeReaction (GameBegins #when)
    ]

instance RunMessage TheFool03 where
  runMessage msg a@(TheFool03 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (cardPlayed -> card) _ -> do
      costModifier (attrs.ability 1) iid (ReduceCostOf (CardWithId card.id) 1)
      pure a
    InHand _ (UseThisAbility iid (isSource attrs -> True) 2) -> do
      putCardIntoPlay iid attrs
      pure a
    _ -> TheFool03 <$> liftRunMessage msg attrs
