module Arkham.Enemy.Cards.TerrorOfTheStarsGuardianOfForbiddenPeaks (terrorOfTheStarsGuardianOfForbiddenPeaks) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query (getLead)
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Projection

newtype TerrorOfTheStarsGuardianOfForbiddenPeaks = TerrorOfTheStarsGuardianOfForbiddenPeaks EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

terrorOfTheStarsGuardianOfForbiddenPeaks :: EnemyCard TerrorOfTheStarsGuardianOfForbiddenPeaks
terrorOfTheStarsGuardianOfForbiddenPeaks =
  enemy
    TerrorOfTheStarsGuardianOfForbiddenPeaks
    Cards.terrorOfTheStarsGuardianOfForbiddenPeaks
    (0, Static 3, 0)
    (2, 2)

instance HasModifiersFor TerrorOfTheStarsGuardianOfForbiddenPeaks where
  getModifiersFor (TerrorOfTheStarsGuardianOfForbiddenPeaks a) = do
    x <-
      fromMaybe 0 <$> runMaybeT do
        loc <- MaybeT $ selectOne $ LocationWithInvestigator ActiveInvestigator
        pos <- MaybeT $ field LocationPosition loc
        pure pos.row
    n <- perPlayer 3
    modifySelf a [EnemyFight x, EnemyEvade x, HealthModifier n]

instance HasAbilities TerrorOfTheStarsGuardianOfForbiddenPeaks where
  getAbilities (TerrorOfTheStarsGuardianOfForbiddenPeaks a) =
    extend1 a
      $ restricted a 1 (exists $ InvestigatorAt $ locationWithEnemy a)
      $ forced
      $ RoundEnds #when

instance RunMessage TerrorOfTheStarsGuardianOfForbiddenPeaks where
  runMessage msg e@(TerrorOfTheStarsGuardianOfForbiddenPeaks attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectForMaybeM (locationWithEnemy attrs) \loc -> do
        row <- maybe 0 (.row) <$> field LocationPosition loc
        lead <- getLead
        investigators <- select $ investigatorAt loc
        chooseOrRunOneAtATimeM lead do
          targets investigators \iid -> moveTo_ (attrs.ability 1) iid (LocationInRow row)
      pure e
    _ -> TerrorOfTheStarsGuardianOfForbiddenPeaks <$> liftRunMessage msg attrs
