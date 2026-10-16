{- | "Revelation - Each investigator discards the top 3 cards of their deck and
draws each weakness discarded. If Henry Jekyll is at your location, discard 1
additional card. If Edward Hyde is at your location, discard 2 additional
cards."
-}
module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Treacheries.BrutishCry (brutishCry) where

import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Deck
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Helpers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype BrutishCry = BrutishCry TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brutishCry :: TreacheryCard BrutishCry
brutishCry = treachery BrutishCry Cards.brutishCry

{- | The two additional-discard clauses are read against each discarding
investigator's own location. Jekyll and Hyde are two faces of one card, so only
one of them is ever in play.
-}
additionalDiscards :: HasGame m => InvestigatorId -> m Int
additionalDiscards iid = do
  jekyll <- selectAny $ henryJekyll <> at_ (locationWithInvestigator iid)
  hyde <- selectAny $ edwardHyde <> at_ (locationWithInvestigator iid)
  pure $ (if jekyll then 1 else 0) + (if hyde then 2 else 0)

instance RunMessage BrutishCry where
  runMessage msg t@(BrutishCry attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      eachInvestigator \iid -> do
        n <- additionalDiscards iid
        discardTopOfDeckAndHandle iid attrs (3 + n) attrs
      pure t
    DiscardedTopOfDeck iid cards (isSource attrs -> True) (isTarget attrs -> True) -> do
      let weaknesses = filterCards WeaknessCard cards
      focusCards weaknesses
        $ chooseOneAtATimeM iid
        $ targets weaknesses
        $ drawCardFrom iid (InvestigatorDiscard iid)
      pure t
    _ -> BrutishCry <$> liftRunMessage msg attrs
