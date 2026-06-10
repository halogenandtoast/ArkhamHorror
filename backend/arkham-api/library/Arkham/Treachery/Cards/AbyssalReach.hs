module Arkham.Treachery.Cards.AbyssalReach (abyssalReach) where

import Arkham.Campaigns.GuardiansOfTheAbyss.Helpers
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype AbyssalReach = AbyssalReach TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abyssalReach :: TreacheryCard AbyssalReach
abyssalReach = treachery AbyssalReach Cards.abyssalReach

instance RunMessage AbyssalReach where
  runMessage msg t@(AbyssalReach attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      n <- getStrengthOfTheAbyss
      revelationSkillTest sid iid attrs #willpower (Fixed n)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      case n of
        1 -> do
          clues <- field InvestigatorClues iid
          when (clues > 0) $ placeCluesOnLocation iid attrs 1
        2 -> assignDamageAndHorror iid attrs 1 1
        3 -> roundModifier attrs iid (CannotPlay AnyCard)
        _ | n >= 4 -> placeDoomOnAgendaAndCheckAdvance 1
        _ -> pure ()
      pure t
    _ -> AbyssalReach <$> liftRunMessage msg attrs
