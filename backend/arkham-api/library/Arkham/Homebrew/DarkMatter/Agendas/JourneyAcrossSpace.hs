module Arkham.Homebrew.DarkMatter.Agendas.JourneyAcrossSpace (journeyAcrossSpace) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.DarkMatter.Helpers (crossOffMemories)
import Arkham.Homebrew.DarkMatter.Key
import Arkham.Matcher

{- | Like every Starfall agenda, this prints:

"[free] During a skill test, cross out 1 tally mark next to your 'Memories':
Reduce the difficulty of this test by 1."
-}
newtype JourneyAcrossSpace = JourneyAcrossSpace AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

journeyAcrossSpace :: AgendaCard JourneyAcrossSpace
journeyAcrossSpace = agenda (1, A) JourneyAcrossSpace Cards.journeyAcrossSpace (Static 12)

instance HasAbilities JourneyAcrossSpace where
  getAbilities (JourneyAcrossSpace a) =
    [ restricted
        a
        1
        (DuringSkillTest AnySkillTest <> youExist (investigatorWithRecordCount Memories (atLeast 1)))
        $ FastAbility Free
    ]

instance RunMessage JourneyAcrossSpace where
  runMessage msg a@(JourneyAcrossSpace attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      crossOffMemories iid 1
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) sid (Difficulty (-1))
      pure a
    {- Agenda 1b is the Sol location card:

    "Revelation - Put this location into play.
    Forced - After you enter this location: You are immediately killed."

    Sol's own side of that card (@:dark-matter:244b@) carries the Forced; this
    side only has to put it into play. It enters face up, so reveal it. -}
    AdvanceAgenda (isSide B attrs -> True) -> do
      -- 'reveal' would query a location that is only queued for placement
      placeLocationCard Locations.sol >>= unsafeReveal
      advanceAgendaDeck attrs
      pure a
    _ -> JourneyAcrossSpace <$> liftRunMessage msg attrs
