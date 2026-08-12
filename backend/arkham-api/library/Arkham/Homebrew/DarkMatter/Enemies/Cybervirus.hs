module Arkham.Homebrew.DarkMatter.Enemies.Cybervirus (cybervirus) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Placement

newtype Cybervirus = Cybervirus EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cybervirus :: EnemyCard Cybervirus
cybervirus = enemy Cybervirus Cards.cybervirus

instance HasAbilities Cybervirus where
  getAbilities (Cybervirus a) = case a.placement of
    HiddenInHand iid ->
      [mkAbility a 1 $ forced $ DiscoveringLastClue #after (You <> InvestigatorWithId iid) Anywhere]
    _ -> getAbilities a

instance RunMessage Cybervirus where
  runMessage msg e@(Cybervirus attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      place attrs $ HiddenInHand iid
      pure e
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ EnemySpawnEngagedWith attrs.id (InvestigatorWithId iid)
      pure e
    _ -> Cybervirus <$> liftRunMessage msg attrs
