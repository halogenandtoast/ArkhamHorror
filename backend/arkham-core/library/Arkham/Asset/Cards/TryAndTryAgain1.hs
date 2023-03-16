module Arkham.Asset.Cards.TryAndTryAgain1
  ( tryAndTryAgain1
  , TryAndTryAgain1(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher hiding (SkillCard)
import Arkham.Projection
import Arkham.Skill.Types ( Field (..) )
import Arkham.Timing qualified as Timing

newtype TryAndTryAgain1 = TryAndTryAgain1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tryAndTryAgain1 :: AssetCard TryAndTryAgain1
tryAndTryAgain1 =
  assetWith TryAndTryAgain1 Cards.tryAndTryAgain1 (discardWhenNoUsesL .~ True)

instance HasAbilities TryAndTryAgain1 where
  getAbilities (TryAndTryAgain1 a) =
    [ restrictedAbility a 1 ControlsThis $ ReactionAbility
        (SkillTestResult
          Timing.After
          Anyone
          (SkillTestWithSkill YourSkill)
          (FailureResult AnyValue)
        )
        (ExhaustCost (toTarget a) <> UseCost (AssetWithId $ toId a) Try 1)
    ]

instance RunMessage TryAndTryAgain1 where
  runMessage msg a@(TryAndTryAgain1 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      committedSkillCards <- selectListMapM (field SkillCard)
        $ skillControlledBy iid
      pushAll
        [ FocusCards committedSkillCards
        , chooseOne
          iid
          [ targetLabel
              (toCardId skillCard)
              [ReturnToHand iid (toTarget $ toCardId skillCard)]
          | skillCard <- committedSkillCards
          ]
        , UnfocusCards
        ]
      pure a
    _ -> TryAndTryAgain1 <$> runMessage msg attrs
