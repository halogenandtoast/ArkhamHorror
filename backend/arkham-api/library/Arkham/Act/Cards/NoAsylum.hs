module Arkham.Act.Cards.NoAsylum (noAsylum) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Action qualified as Action
import Arkham.Constants
import Arkham.Helpers.Log
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TheUnspeakableOath.Helpers
import Arkham.Trait

newtype NoAsylum = NoAsylum ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

noAsylum :: ActCard NoAsylum
noAsylum = act (4, A) NoAsylum Cards.noAsylum Nothing

instance HasAbilities NoAsylum where
  getAbilities (NoAsylum x) =
    guard (onSide A x)
      *> [ scenarioI18n
             $ withI18nTooltip "noAsylum.resign"
             $ restricted
               (proxied (LocationMatcherSource $ locationIs Locations.garden) x)
               ResignAbility
               (Here <> not_ (exists $ ReadyEnemy <> EnemyAt YourLocation))
               (ActionAbility [Action.Resign] $ ActionCost 1)
         , restricted x 1 AllUndefeatedInvestigatorsResigned $ Objective $ forced AnyWindow
         ]

instance HasModifiersFor NoAsylum where
  getModifiersFor (NoAsylum attrs) = do
    modifySelect attrs UnrevealedLocation [TraitRestrictedModifier ArkhamAsylum Blank]

instance RunMessage NoAsylum where
  runMessage msg a@(NoAsylum attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    UseCardAbility iid (isProxySource attrs -> True) ResignAbility _ _ -> do
      resign iid
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      tookKeysByForce <- remembered YouTookTheKeysByForce
      push $ if tookKeysByForce then R2 else R3
      pure a
    _ -> NoAsylum <$> liftRunMessage msg attrs
