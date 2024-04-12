module Arkham.Location.Cards.LightSideOfTheMoon (lightSideOfTheMoon, LightSideOfTheMoon (..)) where

import Arkham.GameValue
import Arkham.Helpers.SkillTest qualified as Msg
import Arkham.Id
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner hiding (chooseOne)
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.Scenarios.DarkSideOfTheMoon.Helpers

newtype Meta = Meta {hasUsedSuccess :: [InvestigatorId]}
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype LightSideOfTheMoon = LightSideOfTheMoon LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lightSideOfTheMoon :: LocationCard LightSideOfTheMoon
lightSideOfTheMoon = locationWith LightSideOfTheMoon Cards.lightSideOfTheMoon 5 (PerPlayer 1) (setMeta $ Meta [])

instance HasAbilities LightSideOfTheMoon where
  getAbilities (LightSideOfTheMoon attrs) =
    extendRevealed attrs [restrictedAbility attrs 1 Here actionAbility]

instance RunMessage LightSideOfTheMoon where
  runMessage msg l@(LightSideOfTheMoon attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOne
        iid
        [ SkillLabel s [Msg.beginSkillTest iid (attrs.ability 1) iid s (Fixed 1)]
        | s <- [#intellect, #agility]
        ]
      pure l
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      let meta = toResult @Meta attrs.meta
      if iid `elem` hasUsedSuccess meta
        then pure l
        else do
          let x = n `div` 3
          reduceAlarmLevelBy x (attrs.ability 1) iid
          pure $ LightSideOfTheMoon $ attrs & setMeta (meta {hasUsedSuccess = iid : hasUsedSuccess meta})
    _ -> LightSideOfTheMoon <$> lift (runMessage msg attrs)
