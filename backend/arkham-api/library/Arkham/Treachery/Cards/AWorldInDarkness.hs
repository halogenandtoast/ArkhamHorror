module Arkham.Treachery.Cards.AWorldInDarkness (aWorldInDarkness) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype AWorldInDarkness = AWorldInDarkness TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aWorldInDarkness :: TreacheryCard AWorldInDarkness
aWorldInDarkness = treachery AWorldInDarkness Cards.aWorldInDarkness

instance RunMessage AWorldInDarkness where
  runMessage msg t@(AWorldInDarkness attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      azathoth <- selectJust $ IncludeOmnipotent $ enemyIs Enemies.azathoth
      doom <- field EnemyDoom azathoth
      if doom == 0
        then gainSurge attrs
        else do
          hasResources <- fieldMap InvestigatorResources (>= 1) iid
          canDiscard <- iid <=~> InvestigatorWithDiscardableCard
          repeated doom do
            chooseOneM iid $ withI18n do
              when hasResources do
                countVar 1 $ labeled' "loseResources" $ loseResources iid attrs 1
              when canDiscard do
                countVar 1 $ labeled' "discardCards" $ chooseAndDiscardCard iid attrs
              countVar 1 $ labeled' "takeHorror" $ assignHorror iid attrs 1
              countVar 1 $ labeled' "takeDamage" $ assignDamage iid attrs 1
      pure t
    _ -> AWorldInDarkness <$> liftRunMessage msg attrs
