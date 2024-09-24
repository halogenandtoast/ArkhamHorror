module Arkham.Enemy.Cards.JoyceLittleBookshopOwner (
  joyceLittleBookshopOwner,
  JoyceLittleBookshopOwner (..),
)
where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Id
import Arkham.Matcher
import Arkham.Message.Lifted.Placement

newtype JoyceLittleBookshopOwner = JoyceLittleBookshopOwner EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

joyceLittleBookshopOwner :: EnemyCard JoyceLittleBookshopOwner
joyceLittleBookshopOwner = enemy JoyceLittleBookshopOwner Cards.joyceLittleBookshopOwner (5, Static 3, 3) (0, 1)

instance HasAbilities JoyceLittleBookshopOwner where
  getAbilities (JoyceLittleBookshopOwner a) =
    extend
      a
      [skillTestAbility $ restrictedAbility a 1 OnSameLocation parleyAction_]

instance RunMessage JoyceLittleBookshopOwner where
  runMessage msg e@(JoyceLittleBookshopOwner attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      placeClues attrs attrs =<< perPlayer 1
      place attrs =<< selectJust (LocationWithTitle "The Little Bookshop")
      pure e
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getId
      parley sid iid (attrs.ability 1) attrs #willpower (Fixed 3)
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      moveTokens (attrs.ability 1) attrs iid #clue 1
      doStep 2 msg
      pure e
    DoStep 2 (PassedThisSkillTest _iid (isAbilitySource attrs 1 -> True)) -> do
      when (attrs.token #clue == 0) $ addToVictory attrs
      pure e
    _ -> JoyceLittleBookshopOwner <$> liftRunMessage msg attrs
