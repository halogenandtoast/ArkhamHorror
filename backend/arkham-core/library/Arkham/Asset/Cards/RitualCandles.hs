module Arkham.Asset.Cards.RitualCandles
  ( ritualCandles
  , RitualCandles(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Window qualified as Window

newtype RitualCandles = RitualCandles AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ritualCandles :: AssetCard RitualCandles
ritualCandles = asset RitualCandles Cards.ritualCandles

instance HasAbilities RitualCandles where
  getAbilities (RitualCandles x) =
    [ restrictedAbility x 1 ControlsThis $ ReactionAbility
        (RevealChaosToken
          Timing.When
          You
          (TokenMatchesAny
          $ map TokenFaceIs [Skull, Cultist, Tablet, ElderThing]
          )
        )
        Free
    ]

instance RunMessage RitualCandles where
  runMessage msg a@(RitualCandles attrs) = case msg of
    UseCardAbility iid source 1 (Window.revealedTokens -> tokens) _
      | isSource attrs source -> do
        push $ If
          (Window.RevealTokenAssetAbilityEffect iid tokens (toId attrs))
          [skillTestModifier attrs (InvestigatorTarget iid) (AnySkillValue 1)]
        pure a
    _ -> RitualCandles <$> runMessage msg attrs
