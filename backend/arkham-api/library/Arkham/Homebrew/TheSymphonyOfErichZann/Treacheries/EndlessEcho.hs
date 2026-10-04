module Arkham.Homebrew.TheSymphonyOfErichZann.Treacheries.EndlessEcho (endlessEcho) where

import Arkham.Ability
import Arkham.Trait (toTraits)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Scenario (scenarioField)
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.Traits (pattern Music)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Scenario.Types (Field (ScenarioDiscard))
import Arkham.Treachery.Import.Lifted

newtype EndlessEcho = EndlessEcho TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

endlessEcho :: TreacheryCard EndlessEcho
endlessEcho = treachery EndlessEcho Cards.endlessEcho

instance HasModifiersFor EndlessEcho where
  -- "Attached location gains +1 shroud for each Music treachery in the discard pile."
  getModifiersFor (EndlessEcho a) = case a.placement of
    AttachedToLocation lid -> do
      discards <- scenarioField ScenarioDiscard
      let n = count (member Music . toTraits) discards
      modifySelect a (LocationWithId lid) [ShroudModifier n]
    _ -> pure mempty

instance HasAbilities EndlessEcho where
  -- "[action]: Test willpower (3). If you succeed, discard Endless Echo."
  getAbilities (EndlessEcho a) =
    [restricted a 1 (OnLocation $ locationWithTreachery a.id) actionAbility]

instance RunMessage EndlessEcho where
  runMessage msg t@(EndlessEcho attrs) = runQueueT $ case msg of
    -- "Attach Endless Echo to the location with the most clues."
    Revelation _ (isSource attrs -> True) -> do
      selectOne (LocationWithMostClues Anywhere) >>= traverse_ (attachTreachery attrs)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #willpower (Fixed 3)
      pure t
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> EndlessEcho <$> liftRunMessage msg attrs
