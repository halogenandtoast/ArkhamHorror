module Arkham.Location.Cards.MoonForest (moonForest, MoonForest (..)) where

import Arkham.Evade
import Arkham.GameValue
import Arkham.Id
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner hiding (beginSkillTest)
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.Scenarios.DarkSideOfTheMoon.Helpers

newtype Meta = Meta {hasUsedSuccess :: [InvestigatorId]}
  deriving stock Generic
  deriving anyclass (FromJSON, ToJSON)

newtype MoonForest = MoonForest LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moonForest :: LocationCard MoonForest
moonForest = locationWith MoonForest Cards.moonForest 4 (PerPlayer 1) (setMeta $ Meta [])

instance HasAbilities MoonForest where
  getAbilities (MoonForest attrs) =
    extendRevealed
      attrs
      [ skillTestAbility $ restrictedAbility attrs 1 Here actionAbility
      , playerLimit PerRound
          $ restrictedAbility attrs 2 (Here <> not_ DuringAction)
          $ FastAbility' Free [#evade]
      ]

instance RunMessage MoonForest where
  runMessage msg l@(MoonForest attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 5)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      let meta = toResult @Meta attrs.meta
      if iid `elem` hasUsedSuccess meta
        then pure l
        else do
          reduceAlarmLevel (attrs.ability 1) iid
          pure $ MoonForest $ attrs & setMeta (meta {hasUsedSuccess = iid : hasUsedSuccess meta})
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      pushM $ mkChooseEvade sid iid (attrs.ability 2)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      raiseAlarmLevel (attrs.ability 2) iid
      pure l
    _ -> MoonForest <$> liftRunMessage msg attrs
