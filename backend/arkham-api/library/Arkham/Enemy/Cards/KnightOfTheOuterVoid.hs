module Arkham.Enemy.Cards.KnightOfTheOuterVoid (knightOfTheOuterVoid) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.ForMovement
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype KnightOfTheOuterVoid = KnightOfTheOuterVoid EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

knightOfTheOuterVoid :: EnemyCard KnightOfTheOuterVoid
knightOfTheOuterVoid =
  enemy KnightOfTheOuterVoid Cards.knightOfTheOuterVoid (3, Static 3, 4) (1, 1)
    & setSpawnAt (ConnectedLocation NotForMovement)

instance HasAbilities KnightOfTheOuterVoid where
  getAbilities (KnightOfTheOuterVoid a) =
    extend1 a $ skillTestAbility $ restricted a 1 OnSameLocation parleyAction_

instance RunMessage KnightOfTheOuterVoid where
  runMessage msg e@(KnightOfTheOuterVoid attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      canPlaceDoom <- toId attrs <=~> NotEnemy (EnemyWithModifier CannotPlaceDoomOnThis)
      when canPlaceDoom $ do
        leadChooseOneM $ withI18n $ nameVar attrs do
          countVar 1 $ labeled' "placeDoomOn" $ placeDoom attrs attrs 1
          countVar 2 $ labeled' "placeDoomOn" $ placeDoom attrs attrs 2
      pure e
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseOneM iid do
        for_ [#willpower, #intellect] \kind ->
          skillLabeled kind $ parley sid iid (attrs.ability 1) attrs kind (Fixed 4)
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      when (attrs.token #doom > 0) do
        removeDoom (attrs.ability 1) attrs 1
        placeClues (attrs.ability 1) iid 1
      pure e
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> KnightOfTheOuterVoid <$> liftRunMessage msg attrs
