module Arkham.Types.Act.Cards.Fold
  ( Fold(..)
  , fold
  ) where

import Arkham.Prelude hiding (fold)

import qualified Arkham.Act.Cards as Cards
import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.Action
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target

newtype Fold = Fold ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fold :: ActCard Fold
fold = act (3, A) Fold Cards.fold Nothing

instance HasAbilities env Fold where
  getAbilities i w (Fold x) = withBaseAbilities i w x $ pure $ if onSide A x
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

instance ActRunner env => RunMessage env Fold where
  runMessage msg a@(Fold attrs@ActAttrs {..}) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      a <$ push (AdvanceAct (toId attrs) source)
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      resignedWithPeterClover <- elem (ResignedCardCode $ CardCode "02079")
        <$> getList ()
      let resolution = if resignedWithPeterClover then 3 else 1
      a <$ push (ScenarioResolution $ Resolution resolution)
    UseCardAbility iid (ProxySource _ source) _ 1 _
      | isSource attrs source && actSequence == Act 3 A -> do
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
      | isSource attrs source && actSequence == Act 3 A -> do
        maid <- selectOne (assetIs Cards.peterClover)
        case maid of
          Nothing -> error "this ability should not be able to be used"
          Just aid -> do
            currentClueCount <- unClueCount <$> getCount aid
            requiredClueCount <- getPlayerCountValue (PerPlayer 1)
            push (PlaceClues (AssetTarget aid) 1)
            a <$ when
              (currentClueCount + 1 >= requiredClueCount)
              (push $ TakeControlOfAsset iid aid)
    _ -> Fold <$> runMessage msg attrs
