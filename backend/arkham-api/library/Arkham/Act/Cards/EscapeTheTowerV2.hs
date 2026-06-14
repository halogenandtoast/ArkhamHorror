module Arkham.Act.Cards.EscapeTheTowerV2 (escapeTheTowerV2) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher

newtype EscapeTheTowerV2 = EscapeTheTowerV2 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

escapeTheTowerV2 :: ActCard EscapeTheTowerV2
escapeTheTowerV2 = act (2, A) EscapeTheTowerV2 Cards.escapeTheTowerV2 Nothing

instance HasAbilities EscapeTheTowerV2 where
  getAbilities = actAbilities \a ->
    [ restricted a 1 (youExist $ at_ (locationIs Locations.eastAntechamber))
        $ ActionAbility #resign Nothing (ActionCost 1)
    , restricted a 2 AllUndefeatedInvestigatorsResigned $ Objective $ forced AnyWindow
    ]

instance RunMessage EscapeTheTowerV2 where
  runMessage msg a@(EscapeTheTowerV2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      resign iid
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R2
      pure a
    _ -> EscapeTheTowerV2 <$> liftRunMessage msg attrs
