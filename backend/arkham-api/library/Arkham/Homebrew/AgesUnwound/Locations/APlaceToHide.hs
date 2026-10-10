module Arkham.Homebrew.AgesUnwound.Locations.APlaceToHide (aPlaceToHide) where

import Arkham.Ability
import Arkham.Card.CardType
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype APlaceToHide = APlaceToHide LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | @clues_fixed@ 0: it never holds clues of its own, and act 2b is what puts
1[per_investigator] on it.
-}
aPlaceToHide :: LocationCard APlaceToHide
aPlaceToHide = symbolLabel $ location APlaceToHide Cards.aPlaceToHide 4 (Static 0)

{- | "Forced - When an investigator at this location initiates a skill test
printed on an agenda card: That investigator automatically succeeds."

Agendas only -- unlike Independence Square, which reads act /and/ agenda. All
three agendas print the same end-of-turn [agility] test, so sitting here is how
you stop paying for standing still.
-}
instance HasAbilities APlaceToHide where
  getAbilities (APlaceToHide a) =
    extendRevealed1 a
      $ forcedAbility a 1
      $ InitiatedSkillTest #when (You <> at_ (be a)) #any #any
      $ SkillTestSourceMatches (SourceIsType AgendaType)

instance RunMessage APlaceToHide where
  runMessage msg l@(APlaceToHide attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) sid SkillTestAutomaticallySucceeds
      pure l
    _ -> APlaceToHide <$> liftRunMessage msg attrs
