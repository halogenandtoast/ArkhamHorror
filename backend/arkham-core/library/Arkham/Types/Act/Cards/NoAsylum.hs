module Arkham.Types.Act.Cards.NoAsylum
  ( NoAsylum(..)
  , noAsylum
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Action qualified as Action
import Arkham.Types.Classes
import Arkham.Types.Criteria
import Arkham.Types.Game.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Resolution
import Arkham.Types.ScenarioLogKey
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Trait

newtype NoAsylum = NoAsylum ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

noAsylum :: ActCard NoAsylum
noAsylum = act (4, A) NoAsylum Cards.noAsylum Nothing

instance HasAbilities NoAsylum where
  getAbilities (NoAsylum x) = withBaseAbilities x $ if onSide A x
    then
      [ restrictedAbility
        (ProxySource
          (LocationMatcherSource $ locationIs Locations.garden)
          (toSource x)
        )
        99
        (Here <> Negate
          (EnemyCriteria $ EnemyExists $ ReadyEnemy <> EnemyAt YourLocation)
        )
        (ActionAbility (Just Action.Resign) $ ActionCost 1)
      , restrictedAbility x 1 AllUndefeatedInvestigatorsResigned
      $ Objective
      $ ForcedAbility AnyWindow
      ]
    else []

instance Query LocationMatcher env => HasModifiersFor env NoAsylum where
  getModifiersFor _ (LocationTarget lid) (NoAsylum attrs) = do
    targets <- select UnrevealedLocation
    pure
      [ toModifier attrs (TraitRestrictedModifier ArkhamAsylum Blank)
      | lid `member` targets
      ]
  getModifiersFor _ _ _ = pure []

instance ActRunner env => RunMessage env NoAsylum where
  runMessage msg a@(NoAsylum attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      a <$ push (AdvanceAct (toId attrs) source)
    AdvanceAct aid _ | aid == toId attrs && onSide B attrs -> do
      tookKeysByForce <- member YouTookTheKeysByForce <$> getSet ()
      a <$ push
        (ScenarioResolution $ Resolution $ if tookKeysByForce then 2 else 3)
    _ -> NoAsylum <$> runMessage msg attrs
