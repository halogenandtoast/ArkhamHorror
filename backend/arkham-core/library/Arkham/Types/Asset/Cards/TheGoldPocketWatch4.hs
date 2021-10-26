module Arkham.Types.Asset.Cards.TheGoldPocketWatch4
  ( theGoldPocketWatch4
  , TheGoldPocketWatch4(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Matcher
import Arkham.Types.Timing qualified as Timing
import Arkham.Types.Window (Window(..))
import Arkham.Types.Window qualified as Window

newtype TheGoldPocketWatch4 = TheGoldPocketWatch4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGoldPocketWatch4 :: AssetCard TheGoldPocketWatch4
theGoldPocketWatch4 = asset TheGoldPocketWatch4 Cards.theGoldPocketWatch4

instance HasAbilities TheGoldPocketWatch4 where
  getAbilities (TheGoldPocketWatch4 attrs) =
    [ restrictedAbility attrs 1 OwnsThis
      $ ReactionAbility (PhaseBegins Timing.When AnyPhase) Free
    , restrictedAbility attrs 2 OwnsThis
      $ ReactionAbility (PhaseEnds Timing.When AnyPhase) Free
    ]

instance AssetRunner env => RunMessage env TheGoldPocketWatch4 where
  runMessage msg a@(TheGoldPocketWatch4 attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      a <$ pushAll [RemoveFromGame (toTarget attrs), EndPhase]
    UseCardAbility _ source [Window _ (Window.PhaseEnds phase)] 2 _
      | isSource attrs source -> do
        clearQueue
        a <$ pushAll [RemoveFromGame (toTarget attrs), Begin phase]
    _ -> TheGoldPocketWatch4 <$> runMessage msg attrs
