module Arkham.Asset.Cards.TryAndTryAgain3 (
  tryAndTryAgain3,
  TryAndTryAgain3 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Matcher hiding (SkillCard)
import Arkham.Projection
import Arkham.Skill.Types (Field (..))
import Arkham.Timing qualified as Timing

newtype TryAndTryAgain3 = TryAndTryAgain3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

tryAndTryAgain3 :: AssetCard TryAndTryAgain3
tryAndTryAgain3 = asset TryAndTryAgain3 Cards.tryAndTryAgain3

instance HasAbilities TryAndTryAgain3 where
  getAbilities (TryAndTryAgain3 x) =
    [ restrictedAbility x 1 ControlsThis
        $ ReactionAbility
          ( SkillTestResult
              Timing.After
              Anyone
              (SkillTestWithSkill YourSkill)
              (FailureResult AnyValue)
          )
          (ExhaustCost $ toTarget x)
    ]

instance RunMessage TryAndTryAgain3 where
  runMessage msg a@(TryAndTryAgain3 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      committedSkillCards <-
        selectListMapM (field SkillCard)
          $ skillControlledBy iid
      player <- getPlayer iid
      pushAll
        [ FocusCards committedSkillCards
        , chooseOne
            player
            [ targetLabel
              (toCardId skillCard)
              [ReturnToHand iid (toTarget $ toCardId skillCard)]
            | skillCard <- committedSkillCards
            ]
        , UnfocusCards
        ]
      pure a
    _ -> TryAndTryAgain3 <$> runMessage msg attrs
