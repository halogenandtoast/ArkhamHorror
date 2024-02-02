module Arkham.Act.Cards.AllIn (AllIn (..), allIn) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Helpers
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Classes
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection
import Arkham.Resolution

newtype AllIn = AllIn ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

allIn :: ActCard AllIn
allIn = act (3, A) AllIn Cards.allIn Nothing

instance HasAbilities AllIn where
  getAbilities (AllIn x) =
    withBaseAbilities x
      $ guard (onSide A x)
      *> [ restrictedAbility
            (toProxySource x $ AssetMatcherSource $ assetIs Assets.drFrancisMorgan)
            1
            (Uncontrolled <> OnSameLocation)
            parleyAction_
         , restrictedAbility x 1 AllUndefeatedInvestigatorsResigned
            $ Objective
            $ forced AnyWindow
         ]

instance RunMessage AllIn where
  runMessage msg a@(AllIn attrs@ActAttrs {..}) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push $ AdvanceAct (toId attrs) source AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == actId && onSide B attrs -> do
      resignedWithDrFrancisMorgan <- resignedWith Assets.drFrancisMorgan
      push $ ScenarioResolution $ Resolution $ if resignedWithDrFrancisMorgan then 2 else 1
      pure a
    UseCardAbility iid (ProxySource _ source) 1 _ _ | isSource attrs source && onSide A attrs -> do
      aid <- selectJust $ assetIs Assets.drFrancisMorgan
      push $ parley iid source aid #willpower 3
      pure a
    PassedThisSkillTest iid (isSource attrs -> True) | onSide A attrs -> do
      aid <- selectJust $ assetIs Assets.drFrancisMorgan
      currentClueCount <- field AssetClues aid
      requiredClueCount <- perPlayer 1
      push $ PlaceClues (toAbilitySource attrs 1) (AssetTarget aid) 1
      pushWhen
        (currentClueCount + 1 >= requiredClueCount)
        (TakeControlOfAsset iid aid)
      pure a
    _ -> AllIn <$> runMessage msg attrs
