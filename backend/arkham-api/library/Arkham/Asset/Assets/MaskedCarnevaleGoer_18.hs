module Arkham.Asset.Assets.MaskedCarnevaleGoer_18 (maskedCarnevaleGoer_18) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype MaskedCarnevaleGoer_18 = MaskedCarnevaleGoer_18 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, Targetable)

maskedCarnevaleGoer_18 :: AssetCard MaskedCarnevaleGoer_18
maskedCarnevaleGoer_18 = asset MaskedCarnevaleGoer_18 Cards.maskedCarnevaleGoer_18

instance HasAbilities MaskedCarnevaleGoer_18 where
  getAbilities (MaskedCarnevaleGoer_18 x) =
    [restrictedAbility x 1 OnSameLocation (actionAbilityWithCost $ clueCost 1)]

instance RunMessage MaskedCarnevaleGoer_18 where
  runMessage msg a@(MaskedCarnevaleGoer_18 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      flipOverBy iid (attrs.ability 1) attrs
      pure a
    Flip _ source (isTarget attrs -> True) -> do
      location <- fieldJust AssetLocation (toId attrs)
      let card = lookupCard Enemies.elisabettaMagro (toCardId attrs)
      investigators <- select $ investigatorAt location
      lead <- getLead
      elisabettaMagro <- createEnemyAt card location
      push $ Flipped (toSource attrs) card

      when (isAbilitySource attrs 1 source) do
        chooseOrRunOneAtATimeM lead do
          targets investigators (initiateEnemyAttack elisabettaMagro attrs)
      pure a
    LookAtRevealed iid source (isTarget a -> True) -> do
      let elisabettaMagro = lookupCard Enemies.elisabettaMagro (toCardId attrs)
      lead <- getLead
      focusCard elisabettaMagro $ continue_ lead
      flipOverBy iid source attrs
      pure a
    _ -> MaskedCarnevaleGoer_18 <$> liftRunMessage msg attrs
