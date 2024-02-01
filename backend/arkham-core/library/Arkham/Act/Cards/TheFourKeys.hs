module Arkham.Act.Cards.TheFourKeys (
  TheFourKeys (..),
  theFourKeys,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Classes
import Arkham.Key
import Arkham.Matcher
import Arkham.Resolution

newtype TheFourKeys = TheFourKeys ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

theFourKeys :: ActCard TheFourKeys
theFourKeys = act (3, A) TheFourKeys Cards.theFourKeys Nothing

-- If investigators at the same location control the Puzzle Box, the Skull key,
-- the Cultist key, the Tablet key, and the ElderThing key, advance.
instance HasAbilities TheFourKeys where
  getAbilities (TheFourKeys attrs) =
    [ restrictedAbility
      attrs
      1
      ( InvestigatorExists (HasMatchingAsset $ assetIs Assets.puzzleBox)
          <> InvestigatorExists
            ( InvestigatorAt (LocationWithInvestigator (HasMatchingAsset $ assetIs Assets.puzzleBox))
                <> InvestigatorWithKey SkullKey
            )
          <> InvestigatorExists
            ( InvestigatorAt (LocationWithInvestigator (HasMatchingAsset $ assetIs Assets.puzzleBox))
                <> InvestigatorWithKey CultistKey
            )
          <> InvestigatorExists
            ( InvestigatorAt (LocationWithInvestigator (HasMatchingAsset $ assetIs Assets.puzzleBox))
                <> InvestigatorWithKey TabletKey
            )
          <> InvestigatorExists
            ( InvestigatorAt (LocationWithInvestigator (HasMatchingAsset $ assetIs Assets.puzzleBox))
                <> InvestigatorWithKey ElderThingKey
            )
      )
      $ Objective
      $ ForcedAbility AnyWindow
    | onSide A attrs
    ]

instance RunMessage TheFourKeys where
  runMessage msg a@(TheFourKeys attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ AdvanceAct (toId attrs) (toSource attrs) AdvancedWithOther
      pure a
    AdvanceAct actId _ _ | actId == toId attrs && onSide B attrs -> do
      membersOfTheLodge <- getHasRecord TheInvestigatorsAreMembersOfTheLodge
      push $ ScenarioResolution $ Resolution $ if membersOfTheLodge then 1 else 2
      pure a
    _ -> TheFourKeys <$> runMessage msg attrs
