module Arkham.Location.Cards.ChapultepecPark (chapultepecPark) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ChapultepecPark = ChapultepecPark LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chapultepecPark :: LocationCard ChapultepecPark
chapultepecPark = symbolLabel $ location ChapultepecPark Cards.chapultepecPark 1 (Static 0)

instance HasAbilities ChapultepecPark where
  getAbilities (ChapultepecPark a) =
    extendRevealed
      a
      [ restricted a 1 Here
          $ forced
          $ SkillTestResult #after You (SkillTestWithSkillType #willpower) #failure
      , skillTestAbility $ restricted a 2 Here exploreAction_
      ]

instance RunMessage ChapultepecPark where
  runMessage msg l@(ChapultepecPark attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid attrs 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      exploreTest sid iid (attrs.ability 2) iid #willpower (Fixed 3)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      push $ Explore iid (attrs.ability 2) $ CardWithPrintedLocationSymbol $ locationSymbol attrs
      pure l
    _ -> ChapultepecPark <$> liftRunMessage msg attrs
