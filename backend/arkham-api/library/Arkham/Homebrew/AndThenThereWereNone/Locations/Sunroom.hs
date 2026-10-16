{- | Sunroom -- an extra clue for arriving empty-handed.

"If this investigation is successful, discover an additional clue" is the
'DiscoveredClues' modifier on the investigator, which 'Investigator.Runner' only
adds to a discovery whose action is Investigate -- so declaring it at initiation
already means "only if this investigation succeeds", the same way Deduction does.
-}
module Arkham.Homebrew.AndThenThereWereNone.Locations.Sunroom (sunroom) where

import Arkham.Ability
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AndThenThereWereNone.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype Sunroom = Sunroom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sunroom :: LocationCard Sunroom
sunroom = location Sunroom Cards.sunroom 3 (PerPlayer 2)

instance HasAbilities Sunroom where
  getAbilities (Sunroom a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "sunroom.clue"
      -- "(Group limit once per round.)"
      $ groupLimit PerRound
      -- "if you do not control any clues"
      $ restricted a 1 (youExist InvestigatorWithoutAnyClues)
      $ freeReaction (InitiatedSkillTest #when You #any #any $ WhileInvestigating (be a))

instance RunMessage Sunroom where
  runMessage msg l@(Sunroom attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) iid (DiscoveredClues 1)
      pure l
    _ -> Sunroom <$> liftRunMessage msg attrs
