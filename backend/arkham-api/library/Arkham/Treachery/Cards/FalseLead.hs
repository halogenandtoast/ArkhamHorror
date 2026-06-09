module Arkham.Treachery.Cards.FalseLead (falseLead) where

import Arkham.Card
import Arkham.Matcher
import Arkham.Source
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype FalseLead = FalseLead TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

falseLead :: TreacheryCard FalseLead
falseLead = treachery FalseLead Cards.falseLead

instance RunMessage FalseLead where
  runMessage msg t@(FalseLead attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      noClues <- matches iid $ not_ InvestigatorWithAnyClues
      if noClues
        then gainSurge attrs
        else do
          sid <- getRandom
          revelationSkillTest sid iid attrs #intellect (Fixed 4)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      skillTestCardOptionEdit
        (toCard attrs)
        (optionWhenExists $ InvestigatorWithId iid <> InvestigatorWithAnyClues)
        $ placeCluesOnLocation iid attrs n
      pure t
    _ -> FalseLead <$> liftRunMessage msg attrs
