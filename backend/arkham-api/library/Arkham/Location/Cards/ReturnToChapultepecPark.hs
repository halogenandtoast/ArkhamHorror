module Arkham.Location.Cards.ReturnToChapultepecPark (returnToChapultepecPark) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ReturnToChapultepecPark = ReturnToChapultepecPark LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToChapultepecPark :: LocationCard ReturnToChapultepecPark
returnToChapultepecPark = symbolLabel $ location ReturnToChapultepecPark Cards.returnToChapultepecPark 2 (Static 0)

instance HasAbilities ReturnToChapultepecPark where
  getAbilities (ReturnToChapultepecPark a) =
    extendRevealed
      a
      [ restrictedAbility a 1 Here
          $ forced
          $ SkillTestResult #after You (SkillTestWithSkillType #willpower) #failure
      , skillTestAbility $ restricted a 2 Here exploreAction_
      ]

instance RunMessage ReturnToChapultepecPark where
  runMessage msg l@(ReturnToChapultepecPark attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid attrs 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      exploreTest sid iid (attrs.ability 2) attrs #agility (Fixed 3)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      push $ Explore iid (attrs.ability 2) $ CardWithPrintedLocationSymbol $ locationSymbol attrs
      pure l
    _ -> ReturnToChapultepecPark <$> liftRunMessage msg attrs
