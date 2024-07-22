module Arkham.Treachery.Cards.UnexpectedAmbush (unexpectedAmbush, UnexpectedAmbush (..)) where

import Arkham.Attack
import Arkham.Helpers.Investigator (withLocationOf)
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.SkillTest qualified as Msg
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype UnexpectedAmbush = UnexpectedAmbush TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unexpectedAmbush :: TreacheryCard UnexpectedAmbush
unexpectedAmbush = treachery UnexpectedAmbush Cards.unexpectedAmbush

instance RunMessage UnexpectedAmbush where
  runMessage msg t@(UnexpectedAmbush attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      enemies <- select AnyEnemy
      sid <- getRandom
      if null enemies
        then push $ Msg.assignDamageAndHorror iid attrs 1 1
        else
          chooseOne
            iid
            [SkillLabel s [Msg.revelationSkillTest sid iid attrs s (Fixed 4)] | s <- [#intellect, #agility]]
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      nearestEnemies <- select $ NearestEnemyTo iid AnyEnemy
      withLocationOf iid \location -> do
        chooseOrRunOne
          iid
          [ targetLabel enemy
            $ [ MoveUntil location (toTarget enemy)
              , EnemyEngageInvestigator enemy iid
              ]
            <> [InitiateEnemyAttack $ enemyAttack enemy attrs iid | n >= 3]
          | enemy <- nearestEnemies
          ]
      pure t
    _ -> UnexpectedAmbush <$> liftRunMessage msg attrs
