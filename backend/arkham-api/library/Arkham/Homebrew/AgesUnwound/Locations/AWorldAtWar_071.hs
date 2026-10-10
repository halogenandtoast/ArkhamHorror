module Arkham.Homebrew.AgesUnwound.Locations.AWorldAtWar_071 (aWorldAtWar_071) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (CannotTriggerAbilityMatching))
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Treacheries
import Arkham.Homebrew.AgesUnwound.Scenarios.Unstuck.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype AWorldAtWar_071 = AWorldAtWar_071 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | /A World at War/, the printing that keeps dealing itself an air raid.
aWorldAtWar_071 :: LocationCard AWorldAtWar_071
aWorldAtWar_071 =
  locationWith AWorldAtWar_071 Cards.aWorldAtWar_071 3 (PerPlayer 1)
    $ connectsToL
    .~ ringConnections

{- | "Forced - At the end of your turn, if there is no copy of Aerial Bombardment
attached to A World at War: Search the encounter deck and discard pile for a copy
of Aerial Bombardment and draw it. Do not resolve its forced ability this turn."

The "no copy attached" clause is a condition on the trigger rather than part of
the effect, so it rides the ability's criteria.
-}
instance HasAbilities AWorldAtWar_071 where
  getAbilities (AWorldAtWar_071 a) =
    extendRevealed1 a
      $ restricted
        a
        endOfTurnAbility
        ( Here
            <> thisExists a (not_ $ LocationWithTreachery $ treacheryIs Treacheries.aerialBombardment)
        )
      $ forced
      $ TurnEnds #when You

instance RunMessage AWorldAtWar_071 where
  runMessage msg l@(AWorldAtWar_071 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      {- The copy is drawn, so its own Revelation attaches it here. Its
      end-of-turn Forced is then silenced for the rest of this turn, which is
      what "do not resolve its forced ability this turn" asks for -- the
      leave-attached-location Forced is untouched. -}
      turnModifier iid (attrs.ability 1) iid
        $ CannotTriggerAbilityMatching
          (AbilityIsForcedAbility <> AbilityOnCard (cardIs Treacheries.aerialBombardment))
      findAndDrawEncounterCard iid (cardIs Treacheries.aerialBombardment)
      pure l
    _ -> AWorldAtWar_071 <$> liftRunMessage msg attrs
