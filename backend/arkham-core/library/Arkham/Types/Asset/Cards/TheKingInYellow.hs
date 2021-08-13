module Arkham.Types.Asset.Cards.TheKingInYellow
  ( theKingInYellow
  , TheKingInYellow(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Restriction hiding (PlayCard)
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing

newtype TheKingInYellow = TheKingInYellow AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theKingInYellow :: AssetCard TheKingInYellow
theKingInYellow = handWith
  TheKingInYellow
  Cards.theKingInYellow
  (canLeavePlayByNormalMeansL .~ False)

instance HasActions TheKingInYellow where
  getActions (TheKingInYellow x) =
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
    let minhId = fromJustNote "not owned" $ assetInvestigator attrs
    commitedCardsCount <- length <$> getSetList @CommittedCardId minhId
    pure $ toModifiers
      attrs
      [ CannotPerformSkillTest
      | commitedCardsCount == 1 || commitedCardsCount == 2
      ]
  getModifiersFor _ _ _ = pure []

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env TheKingInYellow where
  runMessage msg a@(TheKingInYellow attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      a <$ push (PlayCard iid (toCardId attrs) Nothing False)
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      a <$ push (Discard $ toTarget attrs)
    _ -> TheKingInYellow <$> runMessage msg attrs
