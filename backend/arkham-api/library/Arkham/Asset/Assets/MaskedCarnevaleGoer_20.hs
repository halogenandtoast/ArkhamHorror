module Arkham.Asset.Assets.MaskedCarnevaleGoer_20 (maskedCarnevaleGoer_20) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype MaskedCarnevaleGoer_20 = MaskedCarnevaleGoer_20 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, Targetable)

maskedCarnevaleGoer_20 :: AssetCard MaskedCarnevaleGoer_20
maskedCarnevaleGoer_20 = asset MaskedCarnevaleGoer_20 Cards.maskedCarnevaleGoer_20

instance HasAbilities MaskedCarnevaleGoer_20 where
  getAbilities (MaskedCarnevaleGoer_20 x) =
    [restricted x 1 OnSameLocation (actionAbilityWithCost $ clueCost 1)]

instance RunMessage MaskedCarnevaleGoer_20 where
  runMessage msg a@(MaskedCarnevaleGoer_20 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      flipOverBy iid (attrs.ability 1) attrs
      pure a
    Flip _ source (isTarget a -> True) -> do
      location <- fieldJust AssetLocation (toId attrs)
      let card = lookupCard Enemies.savioCorvi (toCardId attrs)
      investigators <- select $ investigatorAt location
      lead <- getLead
      savioCorvi <- createEnemyAt card location
      push $ Flipped (toSource attrs) card

      when (isAbilitySource attrs 1 source) do
        chooseOrRunOneAtATimeM lead do
          targets investigators (initiateEnemyAttack savioCorvi attrs)
      pure a
    LookAtRevealed _ _ (isTarget a -> True) -> do
      let savioCorvi = lookupCard Enemies.savioCorvi (toCardId attrs)
      lead <- getLead
      focusCard savioCorvi $ continue_ lead
      pure a
    _ -> MaskedCarnevaleGoer_20 <$> liftRunMessage msg attrs
