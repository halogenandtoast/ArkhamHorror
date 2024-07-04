module Arkham.Treachery.Cards.AcridMiasma (acridMiasma, AcridMiasma (..)) where

import Arkham.Ability
import Arkham.Helpers.Message qualified as Msg
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype AcridMiasma = AcridMiasma TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

acridMiasma :: TreacheryCard AcridMiasma
acridMiasma = treachery AcridMiasma Cards.acridMiasma

instance HasAbilities AcridMiasma where
  getAbilities (AcridMiasma attrs) = case attrs.attached of
    Just (LocationTarget lid) -> [mkAbility attrs 1 $ forced $ Enters #after You (be lid)]
    _ -> []

instance RunMessage AcridMiasma where
  runMessage msg t@(AcridMiasma attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      mLocation <- selectOne $ NearestLocationToYou $ locationWithoutTreachery Cards.acridMiasma
      for_ mLocation $ attachTreachery (toId attrs)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      beginSkillTest iid (attrs.ability 1) iid #willpower (Fixed 2)
      pure t
    -- not revelation but puts card into active
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      moveHunters <- selectMap HunterMove HunterEnemy
      chooseOrRunOne iid
        $ Label "Take 1 damage and 1 horror" [Msg.assignDamageAndHorror iid attrs 1 1]
        : [Label "Resolve the hunter keyword on each enemy in play" moveHunters | notNull moveHunters]
      pure t
    _ -> AcridMiasma <$> liftRunMessage msg attrs
