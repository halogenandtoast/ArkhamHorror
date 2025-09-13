module Arkham.Enemy.Cards.AllosaurusRampagingPredator (allosaurusRampagingPredator) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.FilmFatale.Helpers

newtype AllosaurusRampagingPredator = AllosaurusRampagingPredator EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

allosaurusRampagingPredator :: EnemyCard AllosaurusRampagingPredator
allosaurusRampagingPredator = enemy AllosaurusRampagingPredator Cards.allosaurusRampagingPredator (5, Static 6, 5) (3, 0)

instance HasModifiersFor AllosaurusRampagingPredator where
  getModifiersFor (AllosaurusRampagingPredator a) = do
    n <- perPlayer 2
    modifySelf a [HealthModifier n]

instance HasAbilities AllosaurusRampagingPredator where
  getAbilities (AllosaurusRampagingPredator a) = extend1 a $ mkAbility a 1 $ forced $ EnemyEnters #after YourLocation (be a)

instance RunMessage AllosaurusRampagingPredator where
  runMessage msg e@(AllosaurusRampagingPredator attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assets <- select $ assetControlledBy iid <> #ally <> DiscardableAsset
      chooseOneM iid do
        withI18n
          $ numberVar "damage" 1
          $ numberVar "horror" 1
          $ labeled' "takeDirectDamageAndHorror"
          $ directDamageAndHorror iid (attrs.ability 1) 1 1
        scenarioI18n $ labeledValidate' (notNull assets) "allosaurus.option" do
          chooseTargetM iid assets $ assetDefeated (attrs.ability 1)
      pure e
    _ -> AllosaurusRampagingPredator <$> liftRunMessage msg attrs
