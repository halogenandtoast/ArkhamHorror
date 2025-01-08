module Arkham.Asset.Assets.WoodenSledge (woodenSledge) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Helpers.Modifiers
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
  getModifiersFor (WoodenSledge a) =
    modifySelectWhen a a.controlled (InvestigatorAt a.location) $ map AsIfInHand a.cardsUnderneath

instance HasAbilities WoodenSledge where
  getAbilities (WoodenSledge a) =
    [ storyControlled a 1 (exists $ InvestigatorAt a.location <> can.search.deck)
        $ actionAbilityWithCost (exhaust a)
    ]

instance RunMessage WoodenSledge where
  runMessage msg a@(WoodenSledge attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      search
        iid
        (attrs.ability 1)
        iid
        [fromTopOfDeck 6]
        (basic $ NonWeakness <> hasAnyTrait [Item, Supply])
        (defer attrs IsNotDraw)
      pure a
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      focusCards cards \unfocus -> do
        chooseUpToNM iid 3 "Done placing cards underneath Wooden Sledge" do
          targets cards (placeUnderneath attrs . pure)
        push unfocus
      pure a
    InitiatePlayCard iid card _ _ _ _ | controlledBy attrs iid && card `elem` attrs.cardsUnderneath -> do
      let remaining = deleteFirstMatch (== card) attrs.cardsUnderneath
      addToHand iid [card]
      push msg
      pure $ WoodenSledge $ attrs & cardsUnderneathL .~ remaining
    _ -> WoodenSledge <$> liftRunMessage msg attrs
