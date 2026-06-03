{- HLINT ignore "Use camelCase" -}
module Arkham.Location.Cards.Dormitories_MiskatonicUniversity (dormitories_MiskatonicUniversity) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (dormitories_MiskatonicUniversity)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype Dormitories_MiskatonicUniversity = Dormitories_MiskatonicUniversity LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dormitories_MiskatonicUniversity :: LocationCard Dormitories_MiskatonicUniversity
dormitories_MiskatonicUniversity = location Dormitories_MiskatonicUniversity Cards.dormitories_MiskatonicUniversity 3 (PerPlayer 1)

instance HasAbilities Dormitories_MiskatonicUniversity where
  getAbilities (Dormitories_MiskatonicUniversity a) =
    extendRevealed1 a
      $ playerLimit PerGame
      $ withCriteria
        (mkAbility a 1 actionAbility)
        (Here <> any_ [HealableInvestigator (toSource a) kind You | kind <- [#horror, #damage]])

instance RunMessage Dormitories_MiskatonicUniversity where
  runMessage msg l@(Dormitories_MiskatonicUniversity attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = UseAbilitySource iid (toSource attrs) 1
      healDamage iid source 1
      healHorror iid source 1
      pure l
    _ -> Dormitories_MiskatonicUniversity <$> liftRunMessage msg attrs
