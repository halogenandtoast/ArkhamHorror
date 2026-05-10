module Arkham.Asset.Assets.WoodenSledge (woodenSledge) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Card
import Arkham.Helpers.Modifiers hiding (costModifier)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Strategy
import Arkham.Trait (Trait (Item, Supply))

newtype WoodenSledge = WoodenSledge AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

woodenSledge :: AssetCard WoodenSledge
woodenSledge = asset WoodenSledge Cards.woodenSledge

instance HasModifiersFor WoodenSledge where
  getModifiersFor (WoodenSledge a) = when a.controlled
    $ modifySelectMap a (InvestigatorAt a.location) \iid ->
      flip mapMaybe a.cardsUnderneath \c -> do
        let nonOwner = maybe False (/= iid) c.owner
        guard (not (nonOwner && isSignature c))
        pure $ AsIfInHandFor ForPlay c.id

instance HasAbilities WoodenSledge where
  getAbilities (WoodenSledge a) =
    [ storyControlled a 1 (exists $ InvestigatorAt a.location <> can.search.deck)
        $ actionAbilityWithCost (exhaust a)
    ]

instance RunMessage WoodenSledge where
  runMessage msg a@(WoodenSledge attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select $ affectsColocated iid
      chooseOrRunOneM iid do
        targets investigators \iid' ->
          search
            iid'
            (attrs.ability 1)
            iid'
            [fromTopOfDeck 6]
            (basic $ NonWeakness <> hasAnyTrait [Item, Supply])
            (defer attrs IsNotDraw)
      pure a
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      focusCards cards do
        cardI18n $ scope "woodenSledge" $ chooseUpToNM' iid 3 "donePlacingCards" do
          targets cards (placeUnderneath attrs . only)
      pure a
    _ -> WoodenSledge <$> liftRunMessage msg attrs
