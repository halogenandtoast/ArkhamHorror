module Arkham.Treachery.Cards.CreatureFeature (creatureFeature) where

import Arkham.Helpers.Scenario
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Matcher
import Arkham.SkillTest.Base
import Arkham.Trait (Trait (Creature, Monster))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CreatureFeature = CreatureFeature TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

creatureFeature :: TreacheryCard CreatureFeature
creatureFeature = treachery CreatureFeature Cards.creatureFeature

instance RunMessage CreatureFeature where
  runMessage msg t@(CreatureFeature attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      combinationSkillTestEdit sid iid attrs iid [#intellect, #combat] (Fixed 5) setIsRevelation
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      findTopOfDiscard (hasAnyTrait [Monster, Creature]) >>= \case
        Nothing -> do
          assignHorror iid attrs 1
          gainSurge attrs
        Just enemy -> drawCard iid enemy
      pure t
    _ -> CreatureFeature <$> liftRunMessage msg attrs
