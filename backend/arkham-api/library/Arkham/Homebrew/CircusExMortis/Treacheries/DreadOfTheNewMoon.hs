module Arkham.Homebrew.CircusExMortis.Treacheries.DreadOfTheNewMoon (dreadOfTheNewMoon) where

import Arkham.Calculation
import Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (getAllSealedMoonTokens, moonToken, sealMoonTokenOn)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose (chooseRevelationSkillTest)
import Arkham.Strategy
import Arkham.Treachery.Import.Lifted

newtype DreadOfTheNewMoon = DreadOfTheNewMoon TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dreadOfTheNewMoon :: TreacheryCard DreadOfTheNewMoon
dreadOfTheNewMoon = treachery DreadOfTheNewMoon Cards.dreadOfTheNewMoon

instance RunMessage DreadOfTheNewMoon where
  runMessage msg t@(DreadOfTheNewMoon attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sealed <- length <$> getAllSealedMoonTokens
      if sealed >= 5
        then gainSurge attrs
        else do
          sid <- getRandom
          -- A calculation, not a snapshot: the base difficulty of a test is read
          -- continuously, and a ☾ revealed during this very test seals itself on
          -- the revealer (SealOnRevealerAndRevealAnother), leaving the bag.
          chooseRevelationSkillTest sid iid attrs [#willpower, #agility] (CountChaosTokens moonToken)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      sealMoonTokenOn iid
      -- "search your deck or discard pile": ShuffleBackIn on the deck half is the
      -- "shuffle your deck if it was searched" clause.
      when (n >= 2) $ search iid attrs iid [fromDeck, fromDiscard] (basic WeaknessCard) (DrawFound iid 1)
      pure t
    _ -> DreadOfTheNewMoon <$> liftRunMessage msg attrs
