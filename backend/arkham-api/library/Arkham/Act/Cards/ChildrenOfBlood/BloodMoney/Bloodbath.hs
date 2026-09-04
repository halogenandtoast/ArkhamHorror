module Arkham.Act.Cards.ChildrenOfBlood.BloodMoney.Bloodbath (bloodbath) where

import Arkham.Ability
import Arkham.Act.CardDefs.ChildrenOfBlood.BloodMoney qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.ForMovement
import Arkham.Helpers.Cost (getCanAffordCost, payEffectCost)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers hiding (skillTestModifier)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.SkillTest.Base
import Arkham.Message.Lifted.Move
import Arkham.Trait (Trait (Civilian))

newtype Bloodbath = Bloodbath ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloodbath :: ActCard Bloodbath
bloodbath = act (3, A) Bloodbath Cards.bloodbath Nothing

instance HasAbilities Bloodbath where
  getAbilities = actAbilities \x ->
    [ skillTestAbility $ mkAbility x 1 parleyAction_
    , mkAbility x 2
        $ Objective
        $ forced
        $ IfEnemyDefeated #after Anyone ByAny (EnemyWithTitle "Howard Wilkes")
    , onlyOnce $ restricted x 3 AllUndefeatedInvestigatorsResigned $ Objective $ forced AnyWindow
    ]

instance RunMessage Bloodbath where
  runMessage msg a@(Bloodbath attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      cost <- ClueCost . Static <$> perPlayer 1
      whenM (getCanAffordCost iid (attrs.ability 1) [] [] cost) do
        chooseOneM iid $ withI18n do
          labeled "spendCluesToSucceed" do
            payEffectCost iid attrs cost
            skillTestModifier sid (attrs.ability 1) sid SkillTestAutomaticallySucceeds
          unscoped skip_
      chooseBeginSkillTestEdit sid iid (attrs.ability 1) attrs [#willpower, #intellect] (Fixed 4)
        $ \st -> st {skillTestAction = Just #parley}
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      civilians <- select $ EnemyWithTrait Civilian <> enemyAtLocationWith iid
      for_ civilians \eid -> do
        connected <- select $ ConnectedTo ForMovement (locationWithEnemy eid)
        chooseOrRunOneM iid $ targets connected $ enemyMoveTo (attrs.ability 1) eid
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure $ Bloodbath $ setMeta True attrs
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push $ if toResultDefault False attrs.meta then R1 else R2
      pure a
    _ -> Bloodbath <$> liftRunMessage msg attrs
