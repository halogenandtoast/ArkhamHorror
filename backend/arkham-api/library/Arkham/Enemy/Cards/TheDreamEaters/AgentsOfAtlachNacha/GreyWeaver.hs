module Arkham.Enemy.Cards.TheDreamEaters.AgentsOfAtlachNacha.GreyWeaver (greyWeaver) where

import Arkham.Classes
import Arkham.Enemy.CardDefs.TheDreamEaters.AgentsOfAtlachNacha qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Prelude

newtype GreyWeaver = GreyWeaver EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

greyWeaver :: EnemyCard GreyWeaver
greyWeaver =
  enemyWith GreyWeaver Cards.greyWeaver
    $ preyL
    .~ Prey (InvestigatorWithLowestSkill #agility UneliminatedInvestigator)

instance HasModifiersFor GreyWeaver where
  getModifiersFor (GreyWeaver a) = do
    modifySelectWhen a a.ready (InvestigatorAt $ locationWithEnemy a) [CannotTakeAction #move]

instance RunMessage GreyWeaver where
  runMessage msg (GreyWeaver attrs) = GreyWeaver <$> runMessage msg attrs
