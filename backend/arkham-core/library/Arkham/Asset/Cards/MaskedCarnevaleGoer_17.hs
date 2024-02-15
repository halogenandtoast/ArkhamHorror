module Arkham.Asset.Cards.MaskedCarnevaleGoer_17 (maskedCarnevaleGoer_17, MaskedCarnevaleGoer_17 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Attack
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection

newtype MaskedCarnevaleGoer_17 = MaskedCarnevaleGoer_17 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, Targetable)

maskedCarnevaleGoer_17 :: AssetCard MaskedCarnevaleGoer_17
maskedCarnevaleGoer_17 = asset MaskedCarnevaleGoer_17 Cards.maskedCarnevaleGoer_17

instance HasAbilities MaskedCarnevaleGoer_17 where
  getAbilities (MaskedCarnevaleGoer_17 x) =
    [restrictedAbility x 1 OnSameLocation (actionAbilityWithCost $ ClueCost (Static 1))]

instance RunMessage MaskedCarnevaleGoer_17 where
  runMessage msg a@(MaskedCarnevaleGoer_17 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ Flip iid (attrs.ability 1) (toTarget attrs)
      pure a
    Flip _ source (isTarget attrs -> True) -> do
      location <- fieldJust AssetLocation (toId attrs)
      let donLagorio = lookupCard Enemies.donLagorio (toCardId attrs)
      investigators <- select $ investigatorAt location
      lead <- getLeadPlayer
      (enemyId, createDonLagorio) <- createEnemyAt donLagorio location Nothing
      pushAll [createDonLagorio, Flipped (toSource attrs) donLagorio]
      let shouldAttack = isAbilitySource attrs 1 source && notNull investigators
      pushWhen shouldAttack
        $ chooseOrRunOneAtATime lead
        $ targetLabels investigators (only . EnemyAttack . enemyAttack enemyId attrs)
      pure a
    LookAtRevealed _ _ (isTarget a -> True) -> do
      let donLagorio = lookupCard Enemies.donLagorio (toCardId attrs)
      lead <- getLeadPlayer
      pushAll [FocusCards [donLagorio], chooseOne lead [Label "Continue" [UnfocusCards]]]
      pure a
    _ -> MaskedCarnevaleGoer_17 <$> lift (runMessage msg attrs)
