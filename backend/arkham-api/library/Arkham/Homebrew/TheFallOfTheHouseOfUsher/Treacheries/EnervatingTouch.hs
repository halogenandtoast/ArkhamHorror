module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Treacheries.EnervatingTouch (enervatingTouch) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), inThreatAreaGets)
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype EnervatingTouch = EnervatingTouch TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

enervatingTouch :: TreacheryCard EnervatingTouch
enervatingTouch = treachery EnervatingTouch Cards.enervatingTouch

instance HasModifiersFor EnervatingTouch where
  -- "Treat your investigator's printed text box as if it were blank (except for
  -- Traits)." Blank leaves InvestigatorTraits alone.
  getModifiersFor (EnervatingTouch a) = inThreatAreaGets a [Blank]

instance HasAbilities EnervatingTouch where
  getAbilities (EnervatingTouch a) = [mkAbility a 1 $ forced $ RoundEnds #when]

instance RunMessage EnervatingTouch where
  runMessage msg t@(EnervatingTouch attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #combat (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> EnervatingTouch <$> liftRunMessage msg attrs
