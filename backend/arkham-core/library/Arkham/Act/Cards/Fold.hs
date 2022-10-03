module Arkham.Act.Cards.Fold
  ( Fold(..)
  , fold
  ) where

import Arkham.Prelude hiding ( fold )

import Arkham.Ability
import Arkham.Act.Types
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Helpers
import Arkham.Act.Runner
import Arkham.Action
import Arkham.Asset.Types ( Field (..) )
import Arkham.Asset.Cards qualified as Cards
import Arkham.Classes
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Types ( Field (..) )
import Arkham.SkillType
import Arkham.Source
import Arkham.Target

newtype Fold = Fold ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fold :: ActCard Fold
fold = act (3, A) Fold Cards.fold Nothing

instance HasAbilities Fold where
  getAbilities (Fold x) = withBaseAbilities x $ if onSide A x
    then
      [ restrictedAbility
        (ProxySource
          (AssetMatcherSource $ assetIs Cards.peterClover)
          (toSource x)
        )
        1
        (Unowned <> OnSameLocation)
        (ActionAbility (Just Parley) $ ActionCost 1)
      , restrictedAbility x 1 AllUndefeatedInvestigatorsResigned
      $ Objective
      $ ForcedAbility AnyWindow
      ]
    else []

instance RunMessage Fold where
  runMessage msg a@(Fold attrs@ActAttrs {..}) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source ->
      a <$ push (AdvanceAct (toId attrs) source AdvancedWithOther)
    AdvanceAct aid _ _ | aid == actId && onSide B attrs -> do
      resignedWithPeterClover <- scenarioFieldMap
        ScenarioResignedCardCodes
        (elem "02079")
      let resolution = if resignedWithPeterClover then 3 else 1
      a <$ push (ScenarioResolution $ Resolution resolution)
    UseCardAbility iid (ProxySource _ source) 1 _ _
      | isSource attrs source && actSequence == Sequence 3 A -> do
        maid <- selectOne (assetIs Cards.peterClover)
        case maid of
          Nothing -> error "this ability should not be able to be used"
          Just aid -> a <$ push
            (BeginSkillTest
              iid
              source
              (AssetTarget aid)
              (Just Parley)
              SkillWillpower
              3
            )
    PassedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source && actSequence == Sequence 3 A -> do
        maid <- selectOne (assetIs Cards.peterClover)
        case maid of
          Nothing -> error "this ability should not be able to be used"
          Just aid -> do
            currentClueCount <- field AssetClues aid
            requiredClueCount <- getPlayerCountValue (PerPlayer 1)
            push (PlaceClues (AssetTarget aid) 1)
            a <$ when
              (currentClueCount + 1 >= requiredClueCount)
              (push $ TakeControlOfAsset iid aid)
    _ -> Fold <$> runMessage msg attrs
