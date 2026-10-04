module Arkham.Homebrew.AgainstTheWendigo.Treacheries.ColdSpirit (coldSpirit) where

import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Traits (pattern Mystical)
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype ColdSpirit = ColdSpirit TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coldSpirit :: TreacheryCard ColdSpirit
coldSpirit = treachery ColdSpirit Cards.coldSpirit

instance RunMessage ColdSpirit where
  runMessage msg t@(ColdSpirit attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      -- "You lose -1 [willpower] for this test if you are in a Mystical location."
      sid <- getRandom
      inMystical <- selectAny $ locationWithInvestigator iid <> LocationWithTrait Mystical
      when inMystical $ skillTestModifier sid attrs iid (SkillModifier #willpower (-1))
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ n -> do
      assignHorror iid attrs n
      pure t
    _ -> ColdSpirit <$> liftRunMessage msg attrs
