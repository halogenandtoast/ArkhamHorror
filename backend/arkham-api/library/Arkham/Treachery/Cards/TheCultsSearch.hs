module Arkham.Treachery.Cards.TheCultsSearch (theCultsSearch) where

import Arkham.Enemy.Types (Field (..))
import Arkham.Matcher
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TheCultsSearch = TheCultsSearch TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCultsSearch :: TreacheryCard TheCultsSearch
theCultsSearch = treachery TheCultsSearch Cards.theCultsSearch

instance RunMessage TheCultsSearch where
  runMessage msg t@(TheCultsSearch attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      cultists <- selectWithField EnemyDoom $ InPlayEnemy $ EnemyWithTrait Cultist <> EnemyWithAnyDoom
      if null cultists
        then findAndDrawEncounterCard iid $ #enemy <> CardWithTrait Cultist
        else do
          for_ cultists \(enemy, doom) -> do
            removeDoom attrs enemy doom
            placeDoomOnAgenda doom
          push AdvanceAgendaIfThresholdSatisfied
      pure t
    _ -> TheCultsSearch <$> liftRunMessage msg attrs
