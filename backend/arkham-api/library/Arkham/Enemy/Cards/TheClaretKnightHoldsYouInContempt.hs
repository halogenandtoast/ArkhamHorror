module Arkham.Enemy.Cards.TheClaretKnightHoldsYouInContempt (theClaretKnightHoldsYouInContempt) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Trait (Trait (Coterie))

newtype TheClaretKnightHoldsYouInContempt = TheClaretKnightHoldsYouInContempt EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theClaretKnightHoldsYouInContempt :: EnemyCard TheClaretKnightHoldsYouInContempt
theClaretKnightHoldsYouInContempt =
  enemy
    TheClaretKnightHoldsYouInContempt
    Cards.theClaretKnightHoldsYouInContempt
    (4, Static 4, 3)
    (1, 2)
    & setSpawnAt (FarthestLocationFromYou Anywhere)

instance HasModifiersFor TheClaretKnightHoldsYouInContempt where
  getModifiersFor (TheClaretKnightHoldsYouInContempt a) =
    modifySelect a (EnemyWithTrait Coterie <> not_ (be a)) [EnemyFight 2]

instance HasAbilities TheClaretKnightHoldsYouInContempt where
  getAbilities (TheClaretKnightHoldsYouInContempt a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ EnemyDiscarded #after AnySource (EnemyWithTrait Coterie <> not_ (be a))

instance RunMessage TheClaretKnightHoldsYouInContempt where
  runMessage msg e@(TheClaretKnightHoldsYouInContempt attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      skeys <- select $ ScarletKeyWithPlacement (AttachedToEnemy attrs.id)
      chooseOneAtATimeM iid $ targets skeys shift
      pure e
    _ -> TheClaretKnightHoldsYouInContempt <$> liftRunMessage msg attrs
