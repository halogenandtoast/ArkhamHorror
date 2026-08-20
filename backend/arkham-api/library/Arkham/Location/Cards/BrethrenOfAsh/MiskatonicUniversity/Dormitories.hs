{- HLINT ignore "Use camelCase" -}
module Arkham.Location.Cards.BrethrenOfAsh.MiskatonicUniversity.Dormitories (dormitories) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.CardDefs.BrethrenOfAsh.MiskatonicUniversity qualified as Cards (
  dormitories,
 )
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype Dormitories = Dormitories LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dormitories :: LocationCard Dormitories
dormitories = location Dormitories Cards.dormitories 3 (PerPlayer 1)

instance HasAbilities Dormitories where
  getAbilities (Dormitories a) =
    extendRevealed1 a
      $ playerLimit PerGame
      $ withCriteria
        (mkAbility a 1 actionAbility)
        (Here <> any_ [HealableInvestigator (toSource a) kind You | kind <- [#horror, #damage]])

instance RunMessage Dormitories where
  runMessage msg l@(Dormitories attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = UseAbilitySource iid (toSource attrs) 1
      healDamage iid source 1
      healHorror iid source 1
      pure l
    _ -> Dormitories <$> liftRunMessage msg attrs
