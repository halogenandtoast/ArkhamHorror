module Arkham.Asset.Cards.MaskedCarnevaleGoer_18 (maskedCarnevaleGoer_18, MaskedCarnevaleGoer_18 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Attack
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection

newtype MaskedCarnevaleGoer_18 = MaskedCarnevaleGoer_18 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, Targetable)

maskedCarnevaleGoer_18 :: AssetCard MaskedCarnevaleGoer_18
maskedCarnevaleGoer_18 = asset MaskedCarnevaleGoer_18 Cards.maskedCarnevaleGoer_18

instance HasAbilities MaskedCarnevaleGoer_18 where
  getAbilities (MaskedCarnevaleGoer_18 x) =
    [restrictedAbility x 1 OnSameLocation (actionAbilityWithCost $ ClueCost (Static 1))]

instance RunMessage MaskedCarnevaleGoer_18 where
  runMessage msg a@(MaskedCarnevaleGoer_18 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ Flip iid (attrs.ability 1) (toTarget attrs)
      pure a
    Flip _ source (isTarget attrs -> True) -> do
      location <- fieldJust AssetLocation (toId attrs)
      let elisabettaMagro = lookupCard Enemies.elisabettaMagro (toCardId attrs)
      investigators <- selectList $ investigatorAt location
      lead <- getLeadPlayer
      (enemyId, createElisabettaMagro) <- createEnemyAt elisabettaMagro location Nothing
      pushAll [createElisabettaMagro, Flipped (toSource attrs) elisabettaMagro]

      let shouldAttack = isAbilitySource attrs 1 source && notNull investigators
      pushWhen shouldAttack
        $ chooseOrRunOneAtATime lead
        $ targetLabels investigators (only . EnemyAttack . enemyAttack enemyId attrs)
      pure a
    LookAtRevealed iid source (isTarget a -> True) -> do
      let elisabettaMagro = lookupCard Enemies.elisabettaMagro (toCardId attrs)
      lead <- getLeadPlayer
      pushAll
        [ FocusCards [elisabettaMagro]
        , chooseOne lead [Label "Continue" [UnfocusCards]]
        , Flip iid source (toTarget attrs)
        ]
      pure a
    _ -> MaskedCarnevaleGoer_18 <$> lift (runMessage msg attrs)
