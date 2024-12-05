module Arkham.Enemy.Cards.LegsOfAtlachNacha_350 (legsOfAtlachNacha_350, LegsOfAtlachNacha_350 (..)) where

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

newtype LegsOfAtlachNacha_350 = LegsOfAtlachNacha_350 EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor LegsOfAtlachNacha_350 where
  getModifiersFor (LegsOfAtlachNacha_350 attrs) = do
    x <- maybe (pure 0) (fieldWithDefault 0 LocationShroud) =<< selectOne (locationWithEnemy attrs)
    modifySelf attrs [CannotMakeAttacksOfOpportunity, DoNotExhaustEvaded, Mod.EnemyFight x]

legsOfAtlachNacha_350 :: EnemyCard LegsOfAtlachNacha_350
legsOfAtlachNacha_350 =
  enemyWith
    LegsOfAtlachNacha_350
    Cards.legsOfAtlachNacha_350
    (0, PerPlayer 3, 3)
    (1, 1)
    (asSelfLocationL ?~ "legs4")

instance HasAbilities LegsOfAtlachNacha_350 where
  getAbilities (LegsOfAtlachNacha_350 attrs) = extend attrs [mkAbility attrs 1 $ forced $ Match.EnemyEvaded #when You (be attrs)]

instance RunMessage LegsOfAtlachNacha_350 where
  runMessage msg e@(LegsOfAtlachNacha_350 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      iids <- select $ InvestigatorAt $ locationWithInvestigator iid
      chooseOrRunOneM iid do
        targets iids \iid' -> roundModifier (attrs.ability 1) iid' $ CannotBeAttackedBy (EnemyWithId attrs.id)
      pure e
    Do (Msg.EnemyEvaded _ eid) | eid == attrs.id -> pure e
    _ -> LegsOfAtlachNacha_350 <$> liftRunMessage msg attrs
