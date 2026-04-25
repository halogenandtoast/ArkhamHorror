module Arkham.Act.Cards.StopTheRite (stopTheRIte) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Resolution (Resolution (..))
import Arkham.Matcher

newtype StopTheRite = StopTheRite ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stopTheRIte :: ActCard StopTheRite
stopTheRIte = act (2, A) StopTheRite Cards.stopTheRIte Nothing

instance HasAbilities StopTheRite where
  getAbilities = actAbilities \a ->
    [ mkAbility a 1 $ Objective $ forced $ ifEnemyDefeatedMatch (EnemyWithTitle "Elokoss")
    ]

instance RunMessage StopTheRite where
  runMessage msg a@(StopTheRite attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push $ ScenarioResolution $ Resolution 1
      advanceActDeck attrs
      pure a
    _ -> StopTheRite <$> liftRunMessage msg attrs
