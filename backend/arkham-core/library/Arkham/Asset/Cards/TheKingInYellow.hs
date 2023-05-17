module Arkham.Asset.Cards.TheKingInYellow
  ( theKingInYellow
  , TheKingInYellow(..)
  ) where

import Arkham.Prelude hiding (head)

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Investigator.Types (Field(..))
import Arkham.Matcher hiding (PlayCard)
import Arkham.Placement
import Arkham.Projection
import Arkham.Timing qualified as Timing
import Arkham.Window (defaultWindows)

newtype TheKingInYellow = TheKingInYellow AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theKingInYellow :: AssetCard TheKingInYellow
theKingInYellow = assetWith
  TheKingInYellow
  Cards.theKingInYellow
  (canLeavePlayByNormalMeansL .~ False)

instance HasAbilities TheKingInYellow where
  getAbilities (TheKingInYellow x) =
    [ restrictedAbility x 1 ControlsThis $ ReactionAbility
        (SkillTestResult
            Timing.After
            (You <> ContributedMatchingIcons (AtLeast $ Static 6))
            AnySkillTest
        $ SuccessResult AnyValue
        )
        Free
    ]

instance HasModifiersFor TheKingInYellow where
  getModifiersFor SkillTestTarget (TheKingInYellow attrs) = do
    case assetPlacement attrs of
      InPlayArea minhId -> do
        commitedCardsCount <- fieldMap InvestigatorCommittedCards length minhId
        pure $ toModifiers
          attrs
          [ CannotPerformSkillTest
          | commitedCardsCount == 1 || commitedCardsCount == 2
          ]
      _ -> error "not owned"
  getModifiersFor _ _ = pure []

instance RunMessage TheKingInYellow where
  runMessage msg a@(TheKingInYellow attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ PutCardIntoPlay iid (toCard attrs) Nothing (defaultWindows iid)
      pure a
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ Discard (toAbilitySource attrs 1) (toTarget attrs)
      pure a
    _ -> TheKingInYellow <$> runMessage msg attrs
