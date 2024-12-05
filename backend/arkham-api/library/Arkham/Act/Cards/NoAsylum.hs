module Arkham.Act.Cards.NoAsylum (NoAsylum (..), noAsylum) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Resolution
import Arkham.ScenarioLogKey
import Arkham.Trait

newtype NoAsylum = NoAsylum ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

noAsylum :: ActCard NoAsylum
noAsylum = act (4, A) NoAsylum Cards.noAsylum Nothing

instance HasAbilities NoAsylum where
  getAbilities (NoAsylum x) =
    withBaseAbilities x
      $ guard (onSide A x)
      *> [ restricted
            (proxied (LocationMatcherSource $ locationIs Locations.garden) x)
            99
            (Here <> not_ (exists $ ReadyEnemy <> EnemyAt YourLocation))
            (ActionAbility [Action.Resign] $ ActionCost 1)
         , restricted x 1 AllUndefeatedInvestigatorsResigned
            $ Objective
            $ ForcedAbility AnyWindow
         ]

instance HasModifiersFor NoAsylum where
  getModifiersFor (NoAsylum attrs) = do
    modifySelect attrs UnrevealedLocation [TraitRestrictedModifier ArkhamAsylum Blank]

instance RunMessage NoAsylum where
  runMessage msg a@(NoAsylum attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push (AdvanceAct (toId attrs) source AdvancedWithOther)
      pure a
    UseCardAbility iid (isProxySource attrs -> True) 99 _ _ -> do
      push (Resign iid)
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      tookKeysByForce <- remembered YouTookTheKeysByForce
      push $ ScenarioResolution $ Resolution $ if tookKeysByForce then 2 else 3
      pure a
    _ -> NoAsylum <$> runMessage msg attrs
