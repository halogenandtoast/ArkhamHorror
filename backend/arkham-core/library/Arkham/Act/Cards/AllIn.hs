module Arkham.Act.Cards.AllIn (
  AllIn (..),
  allIn,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Helpers
import Arkham.Act.Runner
import Arkham.Action
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Resolution
import Arkham.SkillType

newtype AllIn = AllIn ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

allIn :: ActCard AllIn
allIn = act (3, A) AllIn Cards.allIn Nothing

instance HasAbilities AllIn where
  getAbilities (AllIn x) =
    withBaseAbilities x
      $ if onSide A x
        then
          [ restrictedAbility
              (toProxySource x $ AssetMatcherSource $ assetIs Assets.drFrancisMorgan)
              1
              (Uncontrolled <> OnSameLocation)
              (ActionAbility (Just Parley) $ ActionCost 1)
          , restrictedAbility x 1 AllUndefeatedInvestigatorsResigned
              $ Objective
              $ ForcedAbility AnyWindow
          ]
        else []

instance RunMessage AllIn where
  runMessage msg a@(AllIn attrs@ActAttrs {..}) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push $ AdvanceAct (toId attrs) source AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == actId && onSide B attrs -> do
      resignedWithDrFrancisMorgan <- resignedWith Assets.drFrancisMorgan
      let resolution = if resignedWithDrFrancisMorgan then 2 else 1
      push $ ScenarioResolution $ Resolution resolution
      pure a
    UseCardAbility iid (ProxySource _ source) 1 _ _
      | isSource attrs source && onSide A attrs -> do
          aid <- selectJust (assetIs Assets.drFrancisMorgan)
          push $ parley iid source (AssetTarget aid) SkillWillpower 3
          pure a
    PassedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _
      | onSide A attrs ->
          do
            aid <- selectJust $ assetIs Assets.drFrancisMorgan
            currentClueCount <- field AssetClues aid
            requiredClueCount <- perPlayer 1
            push $ PlaceClues (toAbilitySource attrs 1) (AssetTarget aid) 1
            when
              (currentClueCount + 1 >= requiredClueCount)
              (push $ TakeControlOfAsset iid aid)
            pure a
    _ -> AllIn <$> runMessage msg attrs
