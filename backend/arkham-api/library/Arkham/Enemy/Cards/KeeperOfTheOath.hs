module Arkham.Enemy.Cards.KeeperOfTheOath (keeperOfTheOath) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Act (getCurrentActStep)
import Arkham.Helpers.Agenda (getCurrentAgendaStep)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Keyword
import Arkham.Ability
import Arkham.Matcher

newtype KeeperOfTheOath = KeeperOfTheOath EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

keeperOfTheOath :: EnemyCard KeeperOfTheOath
keeperOfTheOath = enemy KeeperOfTheOath Cards.keeperOfTheOath (5, Static 3, 3) (1, 1)

instance HasModifiersFor KeeperOfTheOath where
  getModifiersFor (KeeperOfTheOath a) = do
    currentAct <- getCurrentActStep
    currentAgenda <- getCurrentAgendaStep
    modifySelfWhen a (currentAct > currentAgenda) [AddKeyword Hunter]

instance HasAbilities KeeperOfTheOath where
  getAbilities (KeeperOfTheOath a) =
    extend1 a
      $ restricted
        a
        1
        (exists $ at_ (LocationSharesTraitWith $ locationWithEnemy a) <> InvestigatorWithAnyClues)
      $ forced
      $ PhaseEnds #when #enemy

instance RunMessage KeeperOfTheOath where
  runMessage msg e@(KeeperOfTheOath attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach (at_ (LocationSharesTraitWith $ locationWithEnemy attrs) <> InvestigatorWithAnyClues) \iid -> do
        moveTokens (attrs.ability 1) iid attrs #clue 1
      pure e
    _ -> KeeperOfTheOath <$> liftRunMessage msg attrs
