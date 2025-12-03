module Arkham.Enemy.Cards.TheBeastInACowlOfCrimsonWolfInSheepsClothing (theBeastInACowlOfCrimsonWolfInSheepsClothing) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement

newtype TheBeastInACowlOfCrimsonWolfInSheepsClothing
  = TheBeastInACowlOfCrimsonWolfInSheepsClothing EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBeastInACowlOfCrimsonWolfInSheepsClothing
  :: EnemyCard TheBeastInACowlOfCrimsonWolfInSheepsClothing
theBeastInACowlOfCrimsonWolfInSheepsClothing =
  enemy
    TheBeastInACowlOfCrimsonWolfInSheepsClothing
    Cards.theBeastInACowlOfCrimsonWolfInSheepsClothing
    (3, PerPlayer 5, 4)
    (2, 2)

instance HasAbilities TheBeastInACowlOfCrimsonWolfInSheepsClothing where
  getAbilities (TheBeastInACowlOfCrimsonWolfInSheepsClothing a) =
    extend1 a
      $ restricted a 1 (thisExists a $ EnemyWithScarletKey ScarletKeyAny)
      $ forced
      $ SkillTestResult #after You (SkillTestAt $ locationWithEnemy a) #failure

instance RunMessage TheBeastInACowlOfCrimsonWolfInSheepsClothing where
  runMessage msg e@(TheBeastInACowlOfCrimsonWolfInSheepsClothing attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      skeys <- select $ ScarletKeyWithPlacement (AttachedToEnemy attrs.id)
      lead <- getLead
      chooseOneAtATimeM lead $ targets skeys shift
      pure e
    _ -> TheBeastInACowlOfCrimsonWolfInSheepsClothing <$> liftRunMessage msg attrs
