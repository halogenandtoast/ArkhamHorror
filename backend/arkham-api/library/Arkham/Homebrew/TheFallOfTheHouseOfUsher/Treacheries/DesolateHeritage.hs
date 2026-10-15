module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Treacheries.DesolateHeritage (desolateHeritage) where

import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype DesolateHeritage = DesolateHeritage TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

desolateHeritage :: TreacheryCard DesolateHeritage
desolateHeritage = treachery DesolateHeritage Cards.desolateHeritage

instance RunMessage DesolateHeritage where
  runMessage msg t@(DesolateHeritage attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure t
    {- "If you fail, shuffle the encounter discard pile (including this card)
    back into the encounter deck. If you fail by 2 or more, discard a non-story
    asset you control." This card is still in play, so it is removed and
    shuffled in alongside the pile. -}
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      removeTreachery attrs
      shuffleEncounterDiscardBackIn
      shuffleCardsIntoDeck Deck.EncounterDeck [toCard attrs]
      when (n >= 2) $ chooseAndDiscardAssetMatching iid attrs AssetNonStory
      pure t
    _ -> DesolateHeritage <$> liftRunMessage msg attrs
