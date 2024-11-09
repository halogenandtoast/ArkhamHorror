module Arkham.Location.Cards.HallOfSilence (hallOfSilence, HallOfSilence (..)) where

import Arkham.Ability
import Arkham.Helpers.Log (remembered)
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier
import Arkham.ScenarioLogKey

newtype HallOfSilence = HallOfSilence LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hallOfSilence :: LocationCard HallOfSilence
hallOfSilence = location HallOfSilence Cards.hallOfSilence 4 (PerPlayer 1)

instance HasModifiersFor HallOfSilence where
  getModifiersFor target (HallOfSilence attrs) | isTarget attrs target = do
    unlockedTheThirdFloor <- remembered UnlockedTheThirdFloor
    toModifiers attrs [Blocked | not attrs.revealed && not unlockedTheThirdFloor]
  getModifiersFor _ _ = pure []

instance HasAbilities HallOfSilence where
  getAbilities (HallOfSilence a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)
      , restricted a 2 (Here <> HasRemainingCurseTokens)
          $ forced (SkillTestResult #after You AnySkillTest #failure)
      ]

instance RunMessage HallOfSilence where
  runMessage msg l@(HallOfSilence attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeKey attrs PurpleKey
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      addChaosToken #curse
      pure l
    _ -> HallOfSilence <$> liftRunMessage msg attrs
