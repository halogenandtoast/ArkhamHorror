module Arkham.Treachery.Cards.MysteriousChanting (mysteriousChanting) where

import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype MysteriousChanting = MysteriousChanting TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mysteriousChanting :: TreacheryCard MysteriousChanting
mysteriousChanting = treachery MysteriousChanting Cards.mysteriousChanting

instance RunMessage MysteriousChanting where
  runMessage msg t@(MysteriousChanting attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      enemies <- select $ NearestEnemyToFallback iid $ EnemyWithTrait Cultist
      if null enemies
        then findAndDrawEncounterCard iid $ #enemy <> CardWithTrait Cultist
        else chooseTargetM iid enemies \x -> placeDoom attrs x 2
      pure t
    _ -> MysteriousChanting <$> liftRunMessage msg attrs
