module Arkham.Enemy.Cards.LegsOfAtlachNacha_347 (legsOfAtlachNacha_347, LegsOfAtlachNacha_347 (..)) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Matcher qualified as Match
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose
import Arkham.Modifier qualified as Mod
import Arkham.Projection

newtype LegsOfAtlachNacha_347 = LegsOfAtlachNacha_347 EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor LegsOfAtlachNacha_347 where
  getModifiersFor (LegsOfAtlachNacha_347 attrs) = do
    x <- maybe (pure 0) (fieldWithDefault 0 LocationShroud) =<< selectOne (locationWithEnemy attrs)
    modifySelf attrs [CannotMakeAttacksOfOpportunity, DoNotExhaustEvaded, Mod.EnemyFight x]

legsOfAtlachNacha_347 :: EnemyCard LegsOfAtlachNacha_347
legsOfAtlachNacha_347 =
  enemyWith
    LegsOfAtlachNacha_347
    Cards.legsOfAtlachNacha_347
    (0, PerPlayer 3, 3)
    (1, 1)
    (asSelfLocationL ?~ "legs1")

instance HasAbilities LegsOfAtlachNacha_347 where
  getAbilities (LegsOfAtlachNacha_347 attrs) =
    extend1 attrs
      $ mkAbility attrs 1
      $ forced
      $ Match.EnemyEvaded #when You (be attrs)

instance RunMessage LegsOfAtlachNacha_347 where
  runMessage msg e@(LegsOfAtlachNacha_347 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      iids <- select $ InvestigatorAt $ locationWithInvestigator iid
      chooseOrRunOneM iid do
        targets iids \iid' -> roundModifier (attrs.ability 1) iid' $ CannotBeAttackedBy (EnemyWithId attrs.id)
      pure e
    Do (Msg.EnemyEvaded _ eid) | eid == attrs.id -> pure e
    _ -> LegsOfAtlachNacha_347 <$> liftRunMessage msg attrs
