module Arkham.Act.Cards.NoAsylum (
  NoAsylum (..),
  noAsylum,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Resolution
import Arkham.ScenarioLogKey
import Arkham.Trait

newtype NoAsylum = NoAsylum ActAttrs
  deriving anyclass (IsAct)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

noAsylum :: ActCard NoAsylum
noAsylum = act (4, A) NoAsylum Cards.noAsylum Nothing

instance HasAbilities NoAsylum where
  getAbilities (NoAsylum x) =
    withBaseAbilities x
      $ if onSide A x
        then
          [ restrictedAbility
              ( ProxySource
                  (LocationMatcherSource $ locationIs Locations.garden)
                  (toSource x)
              )
              99
              ( Here
                  <> Negate
                    (EnemyCriteria $ EnemyExists $ ReadyEnemy <> EnemyAt YourLocation)
              )
              (ActionAbility [Action.Resign] $ ActionCost 1)
          , restrictedAbility x 1 AllUndefeatedInvestigatorsResigned
              $ Objective
              $ ForcedAbility AnyWindow
          ]
        else []

instance HasModifiersFor NoAsylum where
  getModifiersFor (LocationTarget lid) (NoAsylum attrs) = do
    targets <- select UnrevealedLocation
    pure
      [ toModifier attrs (TraitRestrictedModifier ArkhamAsylum Blank)
      | lid `member` targets
      ]
  getModifiersFor _ _ = pure []

instance RunMessage NoAsylum where
  runMessage msg a@(NoAsylum attrs) = case msg of
    UseCardAbility _ source 1 _ _
      | isSource attrs source ->
          a <$ push (AdvanceAct (toId attrs) source AdvancedWithOther)
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      tookKeysByForce <- remembered YouTookTheKeysByForce
      a
        <$ push
          (ScenarioResolution $ Resolution $ if tookKeysByForce then 2 else 3)
    _ -> NoAsylum <$> runMessage msg attrs
