module Arkham.Homebrew.TheSymphonyOfErichZann.Enemies.MacabreDancers (macabreDancers) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Enemies qualified as Cards
import Arkham.ForMovement
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (enemyMoveTo, moveTo)

newtype MacabreDancers = MacabreDancers EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

macabreDancers :: EnemyCard MacabreDancers
macabreDancers = enemy MacabreDancers Cards.macabreDancers

instance HasAbilities MacabreDancers where
  -- Dealing damage to it drags both of you to a connecting revealed location.
  getAbilities (MacabreDancers a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyTakeDamage #after AnyDamageEffect (be a) AnyValue AnySource

instance RunMessage MacabreDancers where
  runMessage msg e@(MacabreDancers attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      connecting <- select $ ConnectedFrom ForMovement (locationWithEnemy attrs.id) <> RevealedLocation
      chooseOrRunOneM iid $ targets connecting \lid -> do
        moveTo (attrs.ability 1) iid lid
        enemyMoveTo (attrs.ability 1) attrs lid
      pure e
    _ -> MacabreDancers <$> liftRunMessage msg attrs
