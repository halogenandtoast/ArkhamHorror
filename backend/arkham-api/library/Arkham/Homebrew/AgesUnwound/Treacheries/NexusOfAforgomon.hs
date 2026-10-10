module Arkham.Homebrew.AgesUnwound.Treacheries.NexusOfAforgomon (nexusOfAforgomon) where

import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf, modified_)
import Arkham.Helpers.SkillTest (getSkillTestSource, withSkillTest)
import Arkham.Helpers.Source (sourceMatches)
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Trait (Trait (Paradox, Ritual))
import Arkham.Treachery.Import.Lifted

newtype NexusOfAforgomon = NexusOfAforgomon TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Put into play next to the agenda deck by Scenario VI's setup if /the Myriad
raised a powerful warding/, and removed from the game otherwise. It has no
'Revelation': the only copy is placed during setup and never leaves play, so it
is never drawn.
-}
nexusOfAforgomon :: TreacheryCard NexusOfAforgomon
nexusOfAforgomon = treachery NexusOfAforgomon Cards.nexusOfAforgomon

{- | "Nexus of Aforgomon cannot leave play. /
Each skill test printed on a [[Paradox]] treachery gets +1 difficulty. /
Each [[Ritual]] location gets +2 shroud."

The difficulty rider rides the skill test itself ('Difficulty' is read off
@SkillTestTarget@ and the investigator, @Helpers/SkillTest.hs:604@), the way
/Brazier Enchantment/ does it. "Printed on" is literal: @TreacheryTraits@ is
@cdCardTraits@ with no modifier fold (@Game.hs:6604@), so a granted Paradox trait
does not count --- and the read cannot recurse back into this instance.
-}
instance HasModifiersFor NexusOfAforgomon where
  getModifiersFor (NexusOfAforgomon a) = do
    modifySelf a [CannotLeavePlay]
    modifySelect a (LocationWithTrait Ritual) [ShroudModifier 2]
    runMaybeT_ do
      source <- MaybeT getSkillTestSource
      liftGuardM $ sourceMatches source (SourceIsTreacheryEffect $ TreacheryWithTrait Paradox)
      lift $ withSkillTest \st -> modified_ a st [Difficulty 1]

instance RunMessage NexusOfAforgomon where
  runMessage msg (NexusOfAforgomon attrs) = NexusOfAforgomon <$> runMessage msg attrs
