module Arkham.Location.Cards.CatedralMetropolitana (catedralMetropolitana) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Helpers.Cost
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype CatedralMetropolitana = CatedralMetropolitana LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

catedralMetropolitana :: LocationCard CatedralMetropolitana
catedralMetropolitana = setLabel "b" $ location CatedralMetropolitana Cards.catedralMetropolitana 5 (Static 1)

instance HasAbilities CatedralMetropolitana where
  getAbilities (CatedralMetropolitana a) =
    extendRevealed1 a $ skillTestAbility $ restricted a 1 Here $ actionAbilityWithCost (clueCost 1)

instance RunMessage CatedralMetropolitana where
  runMessage msg l@(CatedralMetropolitana attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      n <- getSpendableClueCount [iid]
      when (n > 0) $ chooseOneM iid $ withI18n $ countVar 1 do
        labeled' "spendClues" do
          spendClues iid 1
          skillTestModifier sid (attrs.ability 1) sid (Difficulty (-2))
        labeled' "doNotSpendClues" nothing
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 4)
      pure l
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) succeededBy -> do
      concealedCards <- map toId <$> getConcealedAtAll attrs.id
      let n = 1 + succeededBy `div` 2
      chooseNM iid n $ targets concealedCards $ revealConcealed iid (attrs.ability 1)
      pure l
    _ -> CatedralMetropolitana <$> liftRunMessage msg attrs
