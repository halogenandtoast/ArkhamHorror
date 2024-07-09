module Arkham.Enemy.Cards.LegsOfAtlachNacha_348 (
  legsOfAtlachNacha_348,
  LegsOfAtlachNacha_348 (..),
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

newtype LegsOfAtlachNacha_348 = LegsOfAtlachNacha_348 EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor LegsOfAtlachNacha_348 where
  getModifiersFor target (LegsOfAtlachNacha_348 attrs) | attrs `is` target = do
    x <- maybe (pure 0) (fieldJust LocationShroud) =<< selectOne (locationWithEnemy attrs)
    pure $ toModifiers attrs [CannotMakeAttacksOfOpportunity, DoNotExhaustEvaded, Mod.EnemyFight x]
  getModifiersFor _ _ = pure []

legsOfAtlachNacha_348 :: EnemyCard LegsOfAtlachNacha_348
legsOfAtlachNacha_348 =
  enemyWith
    LegsOfAtlachNacha_348
    Cards.legsOfAtlachNacha_348
    (0, PerPlayer 3, 3)
    (1, 1)
    (asSelfLocationL ?~ "legs2")

instance HasAbilities LegsOfAtlachNacha_348 where
  getAbilities (LegsOfAtlachNacha_348 attrs) = extend attrs [mkAbility attrs 1 $ forced $ Match.EnemyEvaded #when You (be attrs)]

instance RunMessage LegsOfAtlachNacha_348 where
  runMessage msg e@(LegsOfAtlachNacha_348 attrs) = runQueueT $ case msg of
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
    Do (Msg.EnemyEvaded _ eid) | eid == attrs.id -> do
      pure e
    _ -> LegsOfAtlachNacha_348 <$> liftRunMessage msg attrs
