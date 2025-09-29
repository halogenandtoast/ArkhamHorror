module Arkham.Act.Cards.TheFourKeys (theFourKeys) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheCircleUndone.Key
import Arkham.Matcher

newtype TheFourKeys = TheFourKeys ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theFourKeys :: ActCard TheFourKeys
theFourKeys = act (3, A) TheFourKeys Cards.theFourKeys Nothing

-- If investigators at the same location control the Puzzle Box, the Skull key,
-- the Cultist key, the Tablet key, and the ElderThing key, advance.
instance HasAbilities TheFourKeys where
  getAbilities = actAbilities \a ->
    [ base a (not_ IsReturnTo) $ forced AnyWindow
    , base a IsReturnTo $ FastAbility Free
    ]
   where
    base a c action = restricted a 1 (c <> criteria) $ Objective action
    criteria =
      fold
        $ exists (HasMatchingAsset $ assetIs Assets.puzzleBox)
        : [ exists
              $ at_ (LocationWithInvestigator (HasMatchingAsset $ assetIs Assets.puzzleBox))
              <> InvestigatorWithTokenKey k
          | k <- [#skull, #cultist, #tablet, #elderthing]
          ]

instance RunMessage TheFourKeys where
  runMessage msg a@(TheFourKeys attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      membersOfTheLodge <- getHasRecord TheInvestigatorsAreMembersOfTheLodge
      push $ if membersOfTheLodge then R1 else R2
      pure a
    _ -> TheFourKeys <$> liftRunMessage msg attrs
