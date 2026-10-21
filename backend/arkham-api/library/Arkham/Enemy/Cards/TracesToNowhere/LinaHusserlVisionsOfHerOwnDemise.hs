module Arkham.Enemy.Cards.TracesToNowhere.LinaHusserlVisionsOfHerOwnDemise (linaHusserlVisionsOfHerOwnDemise) where

import Arkham.Ability
import Arkham.Enemy.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Story
import Arkham.Matcher
import Arkham.Scenarios.TracesToNowhere.Helpers
import Arkham.Story.CardDefs.TracesToNowhere qualified as Stories

newtype LinaHusserlVisionsOfHerOwnDemise = LinaHusserlVisionsOfHerOwnDemise EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

linaHusserlVisionsOfHerOwnDemise :: EnemyCard LinaHusserlVisionsOfHerOwnDemise
linaHusserlVisionsOfHerOwnDemise = enemy LinaHusserlVisionsOfHerOwnDemise Cards.linaHusserlVisionsOfHerOwnDemise

instance HasAbilities LinaHusserlVisionsOfHerOwnDemise where
  getAbilities (LinaHusserlVisionsOfHerOwnDemise a) =
    extend
      a
      [ culpritShamblerDamage a 1
      , restricted a 2 (OnSameLocation <> (youExist $ InvestigatorEngagedWith (be a))) parleyAction_
      ]

instance RunMessage LinaHusserlVisionsOfHerOwnDemise where
  runMessage msg e@(LinaHusserlVisionsOfHerOwnDemise attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      nonAttackEnemyDamage Nothing (attrs.ability 1) 1 attrs
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      readStory iid attrs Stories.snapOutOfIt
      pure e
    _ -> LinaHusserlVisionsOfHerOwnDemise <$> liftRunMessage msg attrs
