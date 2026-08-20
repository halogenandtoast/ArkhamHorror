module Arkham.Treachery.Cards.TheDrownedCity.CosmicLegacy.CunningMimicry (cunningMimicry) where

import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Monster))
import Arkham.Treachery.CardDefs.TheDrownedCity.CosmicLegacy qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CunningMimicry = CunningMimicry TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cunningMimicry :: TreacheryCard CunningMimicry
cunningMimicry = treachery CunningMimicry Cards.cunningMimicry

instance RunMessage CunningMimicry where
  runMessage msg t@(CunningMimicry attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      monsters <- selectAny $ EnemyWithTrait Monster
      if monsters
        then do
          sid <- getRandom
          revelationSkillTest sid iid attrs #intellect (Fixed 3)
        else gainSurge attrs
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      monsters <- select $ NearestEnemyToFallback iid (EnemyWithTrait Monster)
      chooseOrRunOneM iid $ targets monsters \eid -> initiateEnemyAttack eid attrs iid
      pure t
    _ -> CunningMimicry <$> liftRunMessage msg attrs
