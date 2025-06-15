module Arkham.Enemy.Cards.CarlSanfordIntimidatingPresence (
  carlSanfordIntimidatingPresence,
  CarlSanfordIntimidatingPresence(..),
) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCard)
import Arkham.Matcher

newtype CarlSanfordIntimidatingPresence = CarlSanfordIntimidatingPresence EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

carlSanfordIntimidatingPresence :: EnemyCard CarlSanfordIntimidatingPresence
carlSanfordIntimidatingPresence =
  enemy CarlSanfordIntimidatingPresence Cards.carlSanfordIntimidatingPresence (3, Static 3, 3) (0, 1)

instance HasModifiersFor CarlSanfordIntimidatingPresence where
  getModifiersFor (CarlSanfordIntimidatingPresence a) = modifySelf a [CannotBeDamaged]

instance HasAbilities CarlSanfordIntimidatingPresence where
  getAbilities (CarlSanfordIntimidatingPresence a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ TurnEnds #when (InvestigatorAt $ locationWithEnemy a)

instance RunMessage CarlSanfordIntimidatingPresence where
  runMessage msg e@(CarlSanfordIntimidatingPresence attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ toMessage $ (chooseAndDiscardCard iid (attrs.ability 1)) { discardFilter = NonWeakness }
      pure e
    _ -> CarlSanfordIntimidatingPresence <$> liftRunMessage msg attrs
