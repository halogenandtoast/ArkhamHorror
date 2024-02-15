module Arkham.Asset.Cards.MaskedCarnevaleGoer_19 (maskedCarnevaleGoer_19, MaskedCarnevaleGoer_19 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Attack
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection

newtype MaskedCarnevaleGoer_19 = MaskedCarnevaleGoer_19 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, Targetable)

maskedCarnevaleGoer_19 :: AssetCard MaskedCarnevaleGoer_19
maskedCarnevaleGoer_19 = asset MaskedCarnevaleGoer_19 Cards.maskedCarnevaleGoer_19

instance HasAbilities MaskedCarnevaleGoer_19 where
  getAbilities (MaskedCarnevaleGoer_19 x) =
    [restrictedAbility x 1 OnSameLocation (actionAbilityWithCost $ ClueCost (Static 1))]

instance RunMessage MaskedCarnevaleGoer_19 where
  runMessage msg a@(MaskedCarnevaleGoer_19 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ Flip iid (attrs.ability 1) (toTarget attrs)
      pure a
    Flip _ source (isTarget a -> True) -> do
      location <- fieldJust AssetLocation (toId attrs)
      let salvatoreNeri = lookupCard Enemies.salvatoreNeri (toCardId attrs)
      investigators <- select $ investigatorAt location
      lead <- getLeadPlayer
      (enemyId, createSalvatoreNeri) <- createEnemyAt salvatoreNeri location Nothing
      pushAll [createSalvatoreNeri, Flipped (toSource attrs) salvatoreNeri]

      let shouldAttack = isAbilitySource attrs 1 source && notNull investigators
      pushWhen shouldAttack
        $ chooseOrRunOneAtATime lead
        $ targetLabels investigators (only . EnemyAttack . enemyAttack enemyId attrs)
      pure a
    LookAtRevealed _ _ (isTarget a -> True) -> do
      let salvatoreNeri = lookupCard Enemies.salvatoreNeri (toCardId attrs)
      lead <- getLeadPlayer
      pushAll
        [ FocusCards [salvatoreNeri]
        , chooseOne lead [Label "Continue" [UnfocusCards]]
        ]
      pure a
    _ -> MaskedCarnevaleGoer_19 <$> lift (runMessage msg attrs)
