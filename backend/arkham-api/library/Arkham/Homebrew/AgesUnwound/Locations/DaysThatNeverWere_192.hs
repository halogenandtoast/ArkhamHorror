module Arkham.Homebrew.AgesUnwound.Locations.DaysThatNeverWere_192 (
  daysThatNeverWere_192,
) where

import Arkham.Ability
import Arkham.Card
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.TimeRunsOut.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (revealedL)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype DaysThatNeverWere_192 = DaysThatNeverWere_192 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

daysThatNeverWere_192 :: LocationCard DaysThatNeverWere_192
daysThatNeverWere_192 =
  symbolLabel
    $ locationWith DaysThatNeverWere_192 Cards.daysThatNeverWere_192 3 (PerPlayer 1)
    $ revealedL
    .~ True

{- | "[action] Take 1 mental trauma and exile a card in your hand of level 1 or
higher: Search your collection for a non-exceptional player card of level at most
X, where X is the level of the exiled card. Add that card to your hand. (Limit
once per game.)"

The limit is unqualified, so it is the card's own and not each investigator's --
'groupLimit'. The costs are not expressible as a 'Arkham.Cost.Cost' (there is no
trauma cost, and 'Arkham.Cost.ExileCost' names one fixed target), so the
criterion gates on having something to exile and the handler charges both.
-}
instance HasAbilities DaysThatNeverWere_192 where
  getAbilities (DaysThatNeverWere_192 a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted
        a
        1
        (Here <> exists (You <> HandWith (HasCard $ NonWeakness <> levelOneOrHigher)))
        actionAbility

instance RunMessage DaysThatNeverWere_192 where
  runMessage msg l@(DaysThatNeverWere_192 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sufferMentalTrauma iid 1
      exileable <- select $ inHandOf NotForPlay iid <> basic (NonWeakness <> levelOneOrHigher)
      chooseOneM iid $ for_ exileable \card -> targeting card do
        exile card
        {- "of level at most X, where X is the level of the exiled card" -- read
        off the card the investigator just gave up, so a higher-level exile buys
        a wider search. -}
        chooseCollectionCard
          iid
          (attrs.ability 1)
          (nonExceptionalPlayerCardAtMost (fromMaybe 0 (toCardDef card).level))
      pure l
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (CardCodeTarget code) -> do
      for_ (lookupCardDef code) \def -> do
        card <- genCard def
        addToHand iid (only card)
      pure l
    _ -> DaysThatNeverWere_192 <$> liftRunMessage msg attrs
