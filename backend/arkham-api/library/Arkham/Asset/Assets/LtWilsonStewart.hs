module Arkham.Asset.Assets.LtWilsonStewart (ltWilsonStewart) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Card
import Arkham.Helpers (unDeck)
import Arkham.Helpers.Asset
import Arkham.Investigator.Types (Field (InvestigatorDeck, InvestigatorHand))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Projection
import Arkham.SkillType

newtype LtWilsonStewart = LtWilsonStewart AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ltWilsonStewart :: AssetCard LtWilsonStewart
ltWilsonStewart = ally LtWilsonStewart Cards.ltWilsonStewart (2, 2)

instance HasAbilities LtWilsonStewart where
  getAbilities (LtWilsonStewart a) =
    [ controlled
        a
        1
        (exists $ You <> oneOf [HandWith (HasCard DiscardableCard), can.manipulate.deck <> not_ DeckIsEmpty])
        $ FastAbility (exhaust a)
    ]

instance RunMessage LtWilsonStewart where
  runMessage msg a@(LtWilsonStewart attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      hand <- field InvestigatorHand iid
      canManipulateDeck <- can.manipulate.deck iid
      topOfDeck <-
        if canManipulateDeck
          then map PlayerCard . take 1 . unDeck <$> field InvestigatorDeck iid
          else pure []
      canDrawCards <- can.draw.cards iid
      canGainResources <- can.gain.resources iid
      canHealDamage <- assetCanHaveDamageHealed (attrs.ability 1) attrs.id
      canHealHorror <- assetCanHaveHorrorHealed (attrs.ability 1) attrs.id
      let
        skills card = cdSkills $ toCardDef card
        countIcon s = count (== SkillIcon s) . skills
        healChoice = do
          when canHealDamage do
            labeled "Heal 1 damage from Lt. Wilson Stewart" $ healDamage attrs (attrs.ability 1) 1
          when canHealHorror do
            labeled "Heal 1 horror from Lt. Wilson Stewart" $ healHorror attrs (attrs.ability 1) 1
        wildChoice = do
          labeled "You get +1 skill value for your next skill test" do
            nextSkillTestModifier iid (attrs.ability 1) iid (AnySkillValue 1)
          when canDrawCards do
            labeled "Draw 1 card" $ drawCards iid (attrs.ability 1) 1
          healChoice
          when canGainResources do
            labeled "Gain 1 resource" $ gainResources iid (attrs.ability 1) 1
        go card = do
          let wills = countIcon #willpower card
          when (wills > 0) $ nextSkillTestModifier iid (attrs.ability 1) iid (AnySkillValue wills)
          when canDrawCards do
            replicateM_ (countIcon #intellect card) $ drawCards iid (attrs.ability 1) 1
          when (canHealDamage || canHealHorror) do
            replicateM_ (countIcon #combat card) $ chooseOneM iid healChoice
          when canGainResources do
            placeTokens (attrs.ability 1) iid #resource (countIcon #agility card)
          replicateM_ (count (== WildIcon) (skills card)) $ chooseOneM iid wildChoice
      chooseOneM iid do
        targets hand \card -> do
          discardCard iid (attrs.ability 1) card
          go card
        when canManipulateDeck do
          deckLabeled iid $ for_ topOfDeck \card -> do
            push $ DiscardCard iid (attrs.ability 1) card.id
            go card
      pure a
    _ -> LtWilsonStewart <$> liftRunMessage msg attrs
