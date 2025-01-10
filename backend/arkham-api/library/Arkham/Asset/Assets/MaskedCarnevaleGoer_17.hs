module Arkham.Asset.Assets.MaskedCarnevaleGoer_17 (maskedCarnevaleGoer_17) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype MaskedCarnevaleGoer_17 = MaskedCarnevaleGoer_17 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, Targetable)

maskedCarnevaleGoer_17 :: AssetCard MaskedCarnevaleGoer_17
maskedCarnevaleGoer_17 = asset MaskedCarnevaleGoer_17 Cards.maskedCarnevaleGoer_17

instance HasAbilities MaskedCarnevaleGoer_17 where
  getAbilities (MaskedCarnevaleGoer_17 x) =
    [restricted x 1 OnSameLocation (actionAbilityWithCost $ clueCost 1)]

instance RunMessage MaskedCarnevaleGoer_17 where
  runMessage msg a@(MaskedCarnevaleGoer_17 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      flipOverBy iid (attrs.ability 1) attrs
      pure a
    Flip _ source (isTarget attrs -> True) -> do
      location <- fieldJust AssetLocation (toId attrs)
      let card = lookupCard Enemies.donLagorio (toCardId attrs)
      investigators <- select $ investigatorAt location
      lead <- getLead
      donLagorio <- createEnemyAt card location
      push $ Flipped (toSource attrs) card
      when (isAbilitySource attrs 1 source) do
        chooseOrRunOneAtATimeM lead
          $ targets investigators (initiateEnemyAttack donLagorio (attrs.ability 1))
      pure a
    LookAtRevealed _ _ (isTarget a -> True) -> do
      let donLagorio = lookupCard Enemies.donLagorio (toCardId attrs)
      lead <- getLead
      focusCards [donLagorio] $ continue_ lead
      pure a
    _ -> MaskedCarnevaleGoer_17 <$> liftRunMessage msg attrs
