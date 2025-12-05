module Arkham.Location.Cards.MelatisShop (melatisShop) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed.Matcher
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey
import Arkham.Trait (Trait (Geist))

newtype MelatisShop = MelatisShop LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

melatisShop :: LocationCard MelatisShop
melatisShop = symbolLabel $ location MelatisShop Cards.melatisShop 2 (Static 0)

instance HasAbilities MelatisShop where
  getAbilities (MelatisShop a) =
    extendRevealed1 a $ restricted a 1 Here $ actionAbilityWithCost (GroupClueCost (PerPlayer 1) (be a))

instance RunMessage MelatisShop where
  runMessage msg l@(MelatisShop attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      remember PeeredBeyond
      geists <- select $ EnemyWithTrait Geist <> NonWeaknessEnemy
      concealeds <- select ConcealedCardAny
      chooseOneM iid do
        targets geists $ lookAtRevealed iid (attrs.ability 1)
        targets concealeds $ lookAtRevealed iid (attrs.ability 1)
      pure l
    _ -> MelatisShop <$> liftRunMessage msg attrs
