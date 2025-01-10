module Arkham.Asset.Assets.MaskedCarnevaleGoer_19 (maskedCarnevaleGoer_19) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype MaskedCarnevaleGoer_19 = MaskedCarnevaleGoer_19 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, Targetable)

maskedCarnevaleGoer_19 :: AssetCard MaskedCarnevaleGoer_19
maskedCarnevaleGoer_19 = asset MaskedCarnevaleGoer_19 Cards.maskedCarnevaleGoer_19

instance HasAbilities MaskedCarnevaleGoer_19 where
  getAbilities (MaskedCarnevaleGoer_19 x) =
    [restricted x 1 OnSameLocation (actionAbilityWithCost $ clueCost 1)]

instance RunMessage MaskedCarnevaleGoer_19 where
  runMessage msg a@(MaskedCarnevaleGoer_19 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      flipOverBy iid (attrs.ability 1) attrs
      pure a
    Flip _ source (isTarget a -> True) -> do
      location <- fieldJust AssetLocation (toId attrs)
      let card = lookupCard Enemies.salvatoreNeri (toCardId attrs)
      investigators <- select $ investigatorAt location
      lead <- getLead
      salvatoreNeri <- createEnemyAt card location
      push $ Flipped (toSource attrs) card

      when (isAbilitySource attrs 1 source) do
        chooseOrRunOneAtATimeM lead do
          targets investigators (initiateEnemyAttack salvatoreNeri attrs)
      pure a
    LookAtRevealed _ _ (isTarget a -> True) -> do
      let salvatoreNeri = lookupCard Enemies.salvatoreNeri (toCardId attrs)
      lead <- getLead
      focusCard salvatoreNeri $ continue_ lead
      pure a
    _ -> MaskedCarnevaleGoer_19 <$> liftRunMessage msg attrs
