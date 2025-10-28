module Arkham.Enemy.Cards.ArkhamOfficer (arkhamOfficer) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Token

newtype ArkhamOfficer = ArkhamOfficer EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamOfficer :: EnemyCard ArkhamOfficer
arkhamOfficer = enemy ArkhamOfficer Cards.arkhamOfficer (3, Static 3, 2) (1, 0)

instance HasAbilities ArkhamOfficer where
  getAbilities (ArkhamOfficer a) =
    extend
      a
      [ restricted a 1 (exists $ locationWithEnemy a <> LocationWithAnyClues)
          $ forced
          $ PhaseEnds #when #enemy
      , skillTestAbility $ restricted a 2 OnSameLocation parleyAction_
      ]

instance RunMessage ArkhamOfficer where
  runMessage msg e@(ArkhamOfficer attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      location <- selectJust $ locationWithEnemy (toId attrs)
      push $ Msg.MovedClues (attrs.ability 1) (toSource location) (toTarget attrs) 1
      flipCluesToDoom attrs 1
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 2) attrs #willpower (Fixed 3)
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      doom <- fieldMap EnemyTokens (countTokens #doom) (toId attrs)

      chooseOrRunOneM iid do
        unless attrs.exhausted do
          labeled "Automatically evade Arkham Officer" $ automaticallyEvadeEnemy iid attrs
        when (doom > 0) do
          labeled "Flip one of its doom to its clue side and take control of it." do
            flipDoomToClues attrs 1
            push $ Msg.MovedClues (attrs.ability 2) (toSource attrs) (toTarget iid) 1
      pure e
    _ -> ArkhamOfficer <$> liftRunMessage msg attrs
