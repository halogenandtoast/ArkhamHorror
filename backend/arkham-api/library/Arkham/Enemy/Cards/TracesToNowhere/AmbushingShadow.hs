module Arkham.Enemy.Cards.TracesToNowhere.AmbushingShadow (ambushingShadow) where

import Arkham.Ability
import Arkham.Enemy.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.TracesToNowhere.Helpers

newtype AmbushingShadow = AmbushingShadow EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ambushingShadow :: EnemyCard AmbushingShadow
ambushingShadow = enemy AmbushingShadow Cards.ambushingShadow

instance HasAbilities AmbushingShadow where
  getAbilities (AmbushingShadow a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyAttacks #after You AnyEnemyAttack (be a)

instance RunMessage AmbushingShadow where
  runMessage msg e@(AmbushingShadow attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      clues <- field InvestigatorClues iid
      chooseOrRunOneM iid $ scenarioI18n do
        labeledValidate (clues > 0) "ambushingShadow.loseClue" $ removeClues (attrs.ability 1) iid 1
        labeled "ambushingShadow.discardCard" $ randomDiscard iid (attrs.ability 1)
      pure e
    _ -> AmbushingShadow <$> liftRunMessage msg attrs
