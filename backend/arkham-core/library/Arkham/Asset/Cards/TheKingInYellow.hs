module Arkham.Asset.Cards.TheKingInYellow
  ( theKingInYellow
  , TheKingInYellow(..)
  ) where

import Arkham.Prelude hiding (head)

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Card.Id
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Id
import Arkham.Matcher hiding (PlayCard)
import Arkham.Modifier
import Arkham.Target
import Arkham.Timing qualified as Timing

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
    [ restrictedAbility x 1 OwnsThis $ ReactionAbility
        (SkillTestResult
            Timing.After
            (You <> ContributedMatchingIcons (AtLeast $ Static 6))
            AnySkillTest
        $ SuccessResult AnyValue
        )
        Free
    ]

instance HasSet CommittedCardId env InvestigatorId => HasModifiersFor env TheKingInYellow where
  getModifiersFor _ SkillTestTarget (TheKingInYellow attrs) = do
    let minhId = fromJustNote "not owned" $ assetController attrs
    commitedCardsCount <- length <$> getSetList @CommittedCardId minhId
    pure $ toModifiers
      attrs
      [ CannotPerformSkillTest
      | commitedCardsCount == 1 || commitedCardsCount == 2
      ]
  getModifiersFor _ _ _ = pure []

instance AssetRunner env => RunMessage TheKingInYellow where
  runMessage msg a@(TheKingInYellow attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      a <$ push (PutCardIntoPlay iid (toCard attrs) Nothing)
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      a <$ push (Discard $ toTarget attrs)
    _ -> TheKingInYellow <$> runMessage msg attrs
