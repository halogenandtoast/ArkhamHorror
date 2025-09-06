module Arkham.Location.Cards.LightSideOfTheMoon (lightSideOfTheMoon) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Id
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.DarkSideOfTheMoon.Helpers

newtype Meta = Meta {hasUsedSuccess :: [InvestigatorId]}
  deriving stock Generic
  deriving anyclass (FromJSON, ToJSON)

newtype LightSideOfTheMoon = LightSideOfTheMoon LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lightSideOfTheMoon :: LocationCard LightSideOfTheMoon
lightSideOfTheMoon = locationWith LightSideOfTheMoon Cards.lightSideOfTheMoon 5 (PerPlayer 1) (setMeta $ Meta [])

instance HasModifiersFor LightSideOfTheMoon where
  getModifiersFor (LightSideOfTheMoon a) = do
    whenUnrevealed a
      $ blockedWhenAny a
      $ locationIs Cards.cavernsBeneathTheMoonLightSide
      <> LocationWithAnyClues

instance HasAbilities LightSideOfTheMoon where
  getAbilities (LightSideOfTheMoon a) =
    extendRevealed1 a $ skillTestAbility $ restricted a 1 Here actionAbility

instance RunMessage LightSideOfTheMoon where
  runMessage msg l@(LightSideOfTheMoon attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid (attrs.ability 1) iid [#intellect, #agility] (Fixed 1)
      pure l
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      let meta = toResult @Meta attrs.meta
      if iid `elem` hasUsedSuccess meta
        then pure l
        else do
          reduceAlarmLevelBy (n `div` 3) (attrs.ability 1) iid
          pure $ LightSideOfTheMoon $ attrs & setMeta (meta {hasUsedSuccess = iid : hasUsedSuccess meta})
    EndRound -> do
      let meta = toResult @Meta attrs.meta
      pure $ LightSideOfTheMoon $ attrs & setMeta (meta {hasUsedSuccess = []})
    _ -> LightSideOfTheMoon <$> liftRunMessage msg attrs
