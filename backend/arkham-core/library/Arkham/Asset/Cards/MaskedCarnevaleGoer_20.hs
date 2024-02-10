module Arkham.Asset.Cards.MaskedCarnevaleGoer_20 (maskedCarnevaleGoer_20, MaskedCarnevaleGoer_20 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Attack
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection

newtype MaskedCarnevaleGoer_20 = MaskedCarnevaleGoer_20 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, Targetable)

maskedCarnevaleGoer_20 :: AssetCard MaskedCarnevaleGoer_20
maskedCarnevaleGoer_20 = asset MaskedCarnevaleGoer_20 Cards.maskedCarnevaleGoer_20

instance HasAbilities MaskedCarnevaleGoer_20 where
  getAbilities (MaskedCarnevaleGoer_20 x) =
    [restrictedAbility x 1 OnSameLocation (actionAbilityWithCost $ ClueCost (Static 1))]

instance RunMessage MaskedCarnevaleGoer_20 where
  runMessage msg a@(MaskedCarnevaleGoer_20 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ Flip iid (attrs.ability 1) (toTarget attrs)
      pure a
    Flip _ source (isTarget a -> True) -> do
      location <- fieldJust AssetLocation (toId attrs)
      let savioCorvi = lookupCard Enemies.savioCorvi (toCardId attrs)
      investigators <- selectList $ investigatorAt location
      lead <- getLeadPlayer
      (enemyId, createSavioCorvi) <- createEnemyAt savioCorvi location Nothing
      pushAll [createSavioCorvi, Flipped (toSource attrs) savioCorvi]

      let shouldAttack = isAbilitySource attrs 1 source && notNull investigators
      pushWhen shouldAttack
        $ chooseOrRunOneAtATime lead
        $ targetLabels investigators (only . EnemyAttack . enemyAttack enemyId attrs)
      pure a
    LookAtRevealed _ _ (isTarget a -> True) -> do
      let savioCorvi = lookupCard Enemies.savioCorvi (toCardId attrs)
      lead <- getLeadPlayer
      pushAll
        [ FocusCards [savioCorvi]
        , chooseOne lead [Label "Continue" [UnfocusCards]]
        ]
      pure a
    _ -> MaskedCarnevaleGoer_20 <$> lift (runMessage msg attrs)
