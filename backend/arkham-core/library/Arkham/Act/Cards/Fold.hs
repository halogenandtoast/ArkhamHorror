module Arkham.Act.Cards.Fold (
  Fold (..),
  fold,
) where

import Arkham.Prelude hiding (fold)

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Helpers
import Arkham.Act.Runner
import Arkham.Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Types (Field (..))
import Arkham.Classes
import Arkham.Matcher
import Arkham.Projection
import Arkham.SkillType

newtype Fold = Fold ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

fold :: ActCard Fold
fold = act (3, A) Fold Cards.fold Nothing

instance HasAbilities Fold where
  getAbilities (Fold x) =
    withBaseAbilities x
      $ if onSide A x
        then
          [ restrictedAbility
              (toProxySource x $ AssetMatcherSource $ assetIs Cards.peterClover)
              1
              (Uncontrolled <> OnSameLocation)
              (ActionAbility [Parley] $ ActionCost 1)
          , restrictedAbility x 1 AllUndefeatedInvestigatorsResigned
              $ Objective
              $ ForcedAbility AnyWindow
          ]
        else []

instance RunMessage Fold where
  runMessage msg a@(Fold attrs@ActAttrs {..}) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push $ AdvanceAct (toId attrs) source AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == actId && onSide B attrs -> do
      resignedWithPeterClover <- resignedWith Cards.peterClover
      push $ scenarioResolution $ if resignedWithPeterClover then 3 else 1
      pure a
    UseCardAbility iid (ProxySource _ source) 1 _ _
      | isSource attrs source && actSequence == Sequence 3 A -> do
          aid <- selectJust $ assetIs Cards.peterClover
          push $ parley iid source (AssetTarget aid) SkillWillpower 3
          pure a
    PassedSkillTest iid _ source SkillTestInitiatorTarget {} _ _
      | isSource attrs source && actSequence == Sequence 3 A -> do
          aid <- selectJust $ assetIs Cards.peterClover
          currentClueCount <- field AssetClues aid
          requiredClueCount <- perPlayer 1
          push $ PlaceClues (toAbilitySource attrs 1) (AssetTarget aid) 1
          when (currentClueCount + 1 >= requiredClueCount)
            $ push
            $ TakeControlOfAsset iid aid
          pure a
    _ -> Fold <$> runMessage msg attrs
