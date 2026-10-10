module Arkham.Homebrew.AgesUnwound.Treacheries.SkillsOfAnotherLife (skillsOfAnotherLife) where

import Arkham.Capability
import Arkham.Helpers.Message.Discard.Lifted (randomDiscardN)
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Strategy
import Arkham.Treachery.Import.Lifted
import Arkham.Zone

newtype SkillsOfAnotherLife = SkillsOfAnotherLife TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

skillsOfAnotherLife :: TreacheryCard SkillsOfAnotherLife
skillsOfAnotherLife = treachery SkillsOfAnotherLife Cards.skillsOfAnotherLife

{- | "Revelation - Discard 2 random cards from your hand. Search the top three
cards of another investigator's deck for a non-weakness, non-signature card and
draw it. Shuffle the searched deck."
-}
instance RunMessage SkillsOfAnotherLife where
  runMessage msg t@(SkillsOfAnotherLife attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      randomDiscardN iid attrs 2
      others <- select $ not_ (InvestigatorWithId iid) <> can.manipulate.deck
      unless (null others) do
        chooseOrRunOneM iid $ targets others \other ->
          search
            iid
            attrs
            other
            [(FromTopOfDeck 3, ShuffleBackIn)]
            (basic $ NonWeakness <> not_ SignatureCard)
            (DrawFound iid 1)
      pure t
    _ -> SkillsOfAnotherLife <$> liftRunMessage msg attrs
