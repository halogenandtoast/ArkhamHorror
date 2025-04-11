module Arkham.Location.Cards.StudentUnion (studentUnion) where

import Arkham.Ability
import Arkham.Capability
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype StudentUnion = StudentUnion LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

studentUnion :: LocationCard StudentUnion
studentUnion = symbolLabel $ location StudentUnion Cards.studentUnion 1 (Static 2)

instance HasAbilities StudentUnion where
  getAbilities (StudentUnion a) =
    extendRevealed
      a
      [ restricted a 1 (not_ $ exists $ locationIs Cards.dormitories)
          $ forced
          $ RevealLocation #after Anyone (be a)
      , restricted a 2 (Here <> youExist (can.heal.any (a.ability 2))) doubleActionAbility
      ]

instance RunMessage StudentUnion where
  runMessage msg l@(StudentUnion attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      whenM (selectNone $ locationIs Cards.dormitories) do
        placeSetAsideLocation_ Cards.dormitories
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let source = attrs.ability 2
      whenM (canHaveDamageHealed source iid) $ healDamage iid source 1
      whenM (canHaveHorrorHealed source iid) $ healHorror iid source 1
      pure l
    _ -> StudentUnion <$> liftRunMessage msg attrs
