module Arkham.Event.Cards.Stouthearted (stouthearted, Stouthearted (..)) where

import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted hiding (InvestigatorDamage)
import Arkham.Helpers.Window (engagedEnemy)
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection

newtype Stouthearted = Stouthearted EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stouthearted :: EventCard Stouthearted
stouthearted = event Stouthearted Cards.stouthearted

instance RunMessage Stouthearted where
  runMessage msg e@(Stouthearted attrs) = runQueueT $ case msg of
    PlayThisEvent _iid (is attrs -> True) -> do
      let eid = engagedEnemy attrs.windows
      health <- fieldJust EnemyRemainingHealth eid
      doStep (min 2 health) msg
      pure e
    DoStep 0 (PlayThisEvent iid (is attrs -> True)) -> do
      let eid = engagedEnemy attrs.windows
      checkDefeated iid eid
      pure e
    DoStep n msg'@(PlayThisEvent iid (is attrs -> True)) | n > 0 -> do
      let eid = engagedEnemy attrs.windows
      health <- fieldJust EnemyRemainingHealth eid
      damage <- field InvestigatorDamage iid
      horror <- field InvestigatorHorror iid
      if (health > 0)
        then
          chooseOne iid
            $ Label "Done moving damage/horror" [DoStep 0 msg']
            : [ DamageLabel
                iid
                [MoveTokens (toSource attrs) (toSource iid) (toTarget eid) #damage 1, DoStep (n - 1) msg']
              | damage > 0
              ]
              <> [ HorrorLabel
                  iid
                  [ RemoveTokens (toSource attrs) (toTarget iid) #horror 1
                  , PlaceTokens (toSource attrs) (toTarget eid) #damage 1
                  , DoStep (n - 1) msg'
                  ]
                 | horror > 0
                 ]
        else doStep 0 msg'

      pure e
    _ -> Stouthearted <$> liftRunMessage msg attrs
