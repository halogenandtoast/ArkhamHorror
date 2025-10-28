module Arkham.Location.Cards.PalacioErrazuriz (palacioErrazuriz) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Helpers.Cost
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype PalacioErrazuriz = PalacioErrazuriz LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

palacioErrazuriz :: LocationCard PalacioErrazuriz
palacioErrazuriz = setLabel "g" $ location PalacioErrazuriz Cards.palacioErrazuriz 2 (PerPlayer 1)

instance HasAbilities PalacioErrazuriz where
  getAbilities (PalacioErrazuriz a) =
    extendRevealed1 a $ skillTestAbility $ restricted a 1 Here $ actionAbilityWithCost (clueCost 1)

instance RunMessage PalacioErrazuriz where
  runMessage msg l@(PalacioErrazuriz attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      n <- getSpendableClueCount [iid]
      when (n > 0) $ chooseOneM iid $ withI18n $ countVar 1 do
        labeled' "spendClues" do
          spendClues iid 1
          skillTestModifier sid (attrs.ability 1) sid (Difficulty (-2))
        labeled' "doNotSpendClues" nothing
      beginSkillTest sid iid (attrs.ability 1) iid #combat (Fixed 4)
      pure l
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) succeededBy -> do
      concealedCards <- map toId <$> getConcealedAtAll (ForExpose $ toSource attrs) attrs.id
      let n = 1 + succeededBy `div` 2
      chooseNM iid n
        $ targets concealedCards
        $ revealConcealed iid (attrs.ability 1)
      pure l
    _ -> PalacioErrazuriz <$> liftRunMessage msg attrs
