module Arkham.Enemy.Cards.TracesToNowhere.EugenePeelDoubleCrossingCultist (eugenePeelDoubleCrossingCultist) where

import Arkham.Ability
import Arkham.Enemy.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Story
import Arkham.Matcher
import Arkham.Scenarios.TracesToNowhere.Helpers
import Arkham.Story.CardDefs.TracesToNowhere qualified as Stories

newtype EugenePeelDoubleCrossingCultist = EugenePeelDoubleCrossingCultist EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eugenePeelDoubleCrossingCultist :: EnemyCard EugenePeelDoubleCrossingCultist
eugenePeelDoubleCrossingCultist = enemy EugenePeelDoubleCrossingCultist Cards.eugenePeelDoubleCrossingCultist

instance HasAbilities EugenePeelDoubleCrossingCultist where
  getAbilities (EugenePeelDoubleCrossingCultist a) =
    extend
      a
      [ culpritShamblerDamage a 1
      , restricted a 2 (OnSameLocation <> (thisExists a ExhaustedEnemy)) parleyAction_
      ]

instance RunMessage EugenePeelDoubleCrossingCultist where
  runMessage msg e@(EugenePeelDoubleCrossingCultist attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      nonAttackEnemyDamage Nothing (attrs.ability 1) 1 attrs
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      readStory iid attrs Stories.youreComingWithMe
      pure e
    _ -> EugenePeelDoubleCrossingCultist <$> liftRunMessage msg attrs
