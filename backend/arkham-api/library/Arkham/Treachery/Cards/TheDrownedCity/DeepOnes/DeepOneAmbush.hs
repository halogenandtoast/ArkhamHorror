module Arkham.Treachery.Cards.TheDrownedCity.DeepOnes.DeepOneAmbush (deepOneAmbush) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Trait (Trait (DeepOne))
import Arkham.Treachery.CardDefs.TheDrownedCity.DeepOnes qualified as Cards
import Arkham.Treachery.Import.Lifted
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype DeepOneAmbush = DeepOneAmbush TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deepOneAmbush :: TreacheryCard DeepOneAmbush
deepOneAmbush = treachery DeepOneAmbush Cards.deepOneAmbush

instance HasAbilities DeepOneAmbush where
  getAbilities (DeepOneAmbush a) =
    [restricted a 1 (InThreatAreaOf You) $ forced $ EnemyEngaged #after You (EnemyWithTrait DeepOne)]

instance RunMessage DeepOneAmbush where
  runMessage msg t@(DeepOneAmbush attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      other <- selectAny $ TreacheryInThreatAreaOf (be iid) <> treacheryIs Cards.deepOneAmbush
      if other then toDiscard attrs attrs else placeInThreatArea attrs iid
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 [Window _ (Window.EnemyEngaged _ enemyId) _ _] _ -> do
      initiateEnemyAttack enemyId (attrs.ability 1) iid
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> DeepOneAmbush <$> liftRunMessage msg attrs
