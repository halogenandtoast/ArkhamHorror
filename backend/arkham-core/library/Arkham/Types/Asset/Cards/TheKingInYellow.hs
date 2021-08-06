module Arkham.Types.Asset.Cards.TheKingInYellow
  ( theKingInYellow
  , TheKingInYellow(..)
  ) where

import Arkham.Prelude hiding (head)

import Prelude (head)

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Window
import Data.Function (on)

newtype TheKingInYellow = TheKingInYellow AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theKingInYellow :: AssetCard TheKingInYellow
theKingInYellow = handWith
  TheKingInYellow
  Cards.theKingInYellow
  (canLeavePlayByNormalMeansL .~ False)

frequency :: Ord a => [a] -> [(a, Int)]
frequency = map (head &&& length) . group . sort

instance HasList CommittedSkillIcon env InvestigatorId => HasActions env TheKingInYellow where
  getActions iid AfterPassSkillTest{} (TheKingInYellow attrs)
    | ownedBy attrs iid = do
      committedSkillIcons <- map unCommittedSkillIcon <$> getList iid
      let frequencies = frequency committedSkillIcons
      case frequencies of
        [] -> pure []
        (x : xs) -> do
          let
            mostFrequent = maximumBy (compare `on` snd) (ncons x xs)
            frequencyMap :: Map SkillType Int = mapFromList frequencies
            totalMatchingIcons = case mostFrequent of
              (SkillWild, n) -> n
              (_, n) -> n + findWithDefault 0 SkillWild frequencyMap
          pure
            [ UseAbility iid (mkAbility attrs 1 $ ReactionAbility Free)
            | totalMatchingIcons >= 6
            ]
  getActions _ _ _ = pure []

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
