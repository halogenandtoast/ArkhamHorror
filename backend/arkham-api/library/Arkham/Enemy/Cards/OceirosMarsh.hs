module Arkham.Enemy.Cards.OceirosMarsh (oceirosMarsh, OceirosMarsh (..)) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks, EnemyEvaded)
import Arkham.Enemy.Types (Field (EnemyKeys))
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Projection ()
import Arkham.Key
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.SkillTest.Base
import Arkham.SkillTestResult

newtype OceirosMarsh = OceirosMarsh EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oceirosMarsh :: EnemyCard OceirosMarsh
oceirosMarsh = enemy OceirosMarsh Cards.oceirosMarsh (4, Static 6, 2) (1, 1)

instance HasModifiersFor OceirosMarsh where
  getModifiersFor (OceirosMarsh a) = do
    atFlooded <- a.id <=~> EnemyAt FloodedLocation
    modifySelfWhen a atFlooded [EnemyEvade 2]

instance HasAbilities OceirosMarsh where
  getAbilities (OceirosMarsh a) =
    extend
      a
      [ restricted a 1 (thisExists a EnemyWithAnyKey)
          $ forced
          $ SkillTestResult #after You (whileEvading a) (SuccessResult $ atLeast 1)
      , restricted a 2 (DuringPhase #enemy) $ forced $ EnemyAttacks #after You AnyEnemyAttack (be a)
      ]

instance RunMessage OceirosMarsh where
  runMessage msg e@(OceirosMarsh attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      mskillTest <- getSkillTest
      case skillTestResult <$> mskillTest of
        Just (SucceededBy _ n) -> do
          ks <- field EnemyKeys attrs.id
          chooseNM iid (min n (length ks)) do
            for_ ks \k ->
              labeled ("Take " <> keyName k) $ placeKey iid k
        _ -> pure ()
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      ks <- iid.keys
      for_ ks (placeKey attrs)
      forInvestigator iid $ ScenarioSpecific "captured" Null
      pure e
    _ -> OceirosMarsh <$> liftRunMessage msg attrs
