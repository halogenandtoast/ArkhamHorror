module Arkham.Treachery.Cards.ChildrenOfBlood.Stalked.VoicesInTheNight (voicesInTheNight) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Treachery.CardDefs.ChildrenOfBlood.Stalked qualified as Cards
import Arkham.Treachery.Import.Lifted
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype VoicesInTheNight = VoicesInTheNight TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

voicesInTheNight :: TreacheryCard VoicesInTheNight
voicesInTheNight = treachery VoicesInTheNight Cards.voicesInTheNight

instance HasAbilities VoicesInTheNight where
  getAbilities (VoicesInTheNight a) =
    [ restricted a 1 (InThreatAreaOf You) $ forced $ EnemyEngaged #after You AnyEnemy
    , skillTestAbility $ restricted a 2 OnSameLocation actionAbility
    ]

instance RunMessage VoicesInTheNight where
  runMessage msg t@(VoicesInTheNight attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 [Window _ (Window.EnemyEngaged _ enemyId) _] _ -> do
      toDiscardBy iid (attrs.ability 1) attrs
      initiateEnemyAttack enemyId (attrs.ability 1) iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #willpower (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> VoicesInTheNight <$> liftRunMessage msg attrs
