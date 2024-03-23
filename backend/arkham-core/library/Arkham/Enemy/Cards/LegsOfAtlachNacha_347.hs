module Arkham.Enemy.Cards.LegsOfAtlachNacha_347 (
  legsOfAtlachNacha_347,
  LegsOfAtlachNacha_347 (..),
)
where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers qualified as Msg
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Matcher qualified as Match
import Arkham.Message qualified as Msg
import Arkham.Modifier (ModifierType (..))
import Arkham.Modifier qualified as Mod
import Arkham.Projection

newtype LegsOfAtlachNacha_347 = LegsOfAtlachNacha_347 EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor LegsOfAtlachNacha_347 where
  getModifiersFor target (LegsOfAtlachNacha_347 attrs) | attrs `is` target = do
    x <- field LocationShroud =<< selectJust (locationWithEnemy attrs)
    pure $ toModifiers attrs [CannotMakeAttacksOfOpportunity, DoNotExhaustEvaded, Mod.EnemyFight x]
  getModifiersFor _ _ = pure []

legsOfAtlachNacha_347 :: EnemyCard LegsOfAtlachNacha_347
legsOfAtlachNacha_347 =
  enemyWith
    LegsOfAtlachNacha_347
    Cards.legsOfAtlachNacha_347
    (0, PerPlayer 3, 3)
    (1, 1)
    (asSelfLocationL ?~ "legs1")

instance HasAbilities LegsOfAtlachNacha_347 where
  getAbilities (LegsOfAtlachNacha_347 attrs) = extend attrs [mkAbility attrs 1 $ forced $ Match.EnemyEvaded #when You (be attrs)]

instance RunMessage LegsOfAtlachNacha_347 where
  runMessage msg e@(LegsOfAtlachNacha_347 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      iids <- select $ InvestigatorAt $ locationWithInvestigator iid
      chooseOrRunOne
        iid
        [ targetLabel
          iid'
          [Msg.roundModifier (attrs.ability 1) iid' $ CannotBeAttackedBy (EnemyWithId attrs.id)]
        | iid' <- iids
        ]
      pure e
    Msg.EnemyEvaded _ eid | eid == attrs.id -> do
      pure e
    _ -> LegsOfAtlachNacha_347 <$> lift (runMessage msg attrs)
