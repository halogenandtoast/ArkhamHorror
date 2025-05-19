module Arkham.Act.Cards.CurtainCall (curtainCall) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Action qualified as Action
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.CurtainCall.Helpers

newtype CurtainCall = CurtainCall ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

curtainCall :: ActCard CurtainCall
curtainCall = act (3, A) CurtainCall Cards.curtainCall Nothing

instance HasAbilities CurtainCall where
  getAbilities (CurtainCall attrs) =
    [ restricted
        (proxied (LocationMatcherSource $ locationIs Cards.lobby) attrs)
        1
        (Here <> not_ (exists $ InPlayEnemy $ enemyIs Enemies.theManInThePallidMask))
        $ ActionAbility [Action.Resign] (ActionCost 1)
    , restricted attrs 2 (exists $ LocationWithoutHorror <> ConnectedTo LocationWithAnyHorror)
        $ forced
        $ RoundEnds #when
    ]
      <> [ restricted attrs 3 AllUndefeatedInvestigatorsResigned $ Objective $ forced AnyWindow
         | onSide A attrs
         ]

instance RunMessage CurtainCall where
  runMessage msg a@(CurtainCall attrs) = runQueueT $ case msg of
    UseThisAbility iid (ProxySource _ (isSource attrs -> True)) 1 -> do
      resign iid
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      selectEach (LocationWithoutHorror <> ConnectedTo LocationWithAnyHorror) \location ->
        placeTokens (attrs.ability 2) location #horror 1
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> scenarioI18n do
      leadChooseOneM do
        labeled' "curtainCall.r1" $ push R1
        labeled' "curtainCall.r2" $ push R2
      pure a
    _ -> CurtainCall <$> liftRunMessage msg attrs
