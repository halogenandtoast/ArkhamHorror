module Arkham.Asset.Assets.LtWilsonStewart (ltWilsonStewart) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Helpers (unDeck)
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
  getAbilities (LtWilsonStewart a) = [controlled a 1 (exists $ HandWith AnyCards) $ FastAbility (exhaust a)]

instance RunMessage LtWilsonStewart where
  runMessage msg a@(LtWilsonStewart attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      hand <- field InvestigatorHand iid
      topOfDeck <- map PlayerCard . take 1 . unDeck <$> field InvestigatorDeck iid
      let
        countIcon s card = count (== SkillIcon s) (cdSkills $ toCardDef card)
        go card = do
          let wills = countIcon #willpower card
          when (wills > 0) $ nextSkillTestModifier iid (attrs.ability 1) iid (AnySkillValue wills)
          replicateM_ (countIcon #intellect card) $ drawCards iid (attrs.ability 1) 1
          replicateM_ (countIcon #combat card) $ chooseOneM iid do
            labeled "Heal 1 damage from Lt. Wilson Stewart" $ healDamage attrs (attrs.ability 1) 1
            labeled "Heal 1 horror from Lt. Wilson Stewart" $ healHorror attrs (attrs.ability 1) 1
          placeTokens (attrs.ability 1) iid #resource (countIcon #agility card)
      chooseOneM iid do
        targets hand \card -> discardCard iid (attrs.ability 1) card >> go card
        targets topOfDeck \card -> push (DiscardCard iid (attrs.ability 1) card.id) >> go card
      pure a
    _ -> LtWilsonStewart <$> liftRunMessage msg attrs
