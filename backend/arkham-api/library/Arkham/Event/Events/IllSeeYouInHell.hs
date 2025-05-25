module Arkham.Event.Events.IllSeeYouInHell (illSeeYouInHell) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher hiding (InvestigatorDefeated)

newtype IllSeeYouInHell = IllSeeYouInHell EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

illSeeYouInHell :: EventCard IllSeeYouInHell
illSeeYouInHell = event IllSeeYouInHell Cards.illSeeYouInHell

instance RunMessage IllSeeYouInHell where
  runMessage msg e@(IllSeeYouInHell attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      enemies <- select $ EnemyIsEngagedWith (InvestigatorWithId iid) <> NonEliteEnemy
      for_ enemies \enemy -> defeatEnemy enemy iid attrs
      investigatorDefeated attrs iid
      sufferPhysicalTrauma iid 1
      pure e
    _ -> IllSeeYouInHell <$> liftRunMessage msg attrs
