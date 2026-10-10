module Arkham.Homebrew.AgesUnwound.Locations.DawnOfTheUniverse_202 (
  dawnOfTheUniverse_202,
) where

import Arkham.Ability
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (revealedL)
import Arkham.Matcher
import Arkham.Strategy

newtype DawnOfTheUniverse_202 = DawnOfTheUniverse_202 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dawnOfTheUniverse_202 :: LocationCard DawnOfTheUniverse_202
dawnOfTheUniverse_202 =
  symbolLabel
    $ locationWith DawnOfTheUniverse_202 Cards.dawnOfTheUniverse_202 4 (PerPlayer 1)
    $ revealedL
    .~ True

{- | "[reaction] After you enter Dawn of the Universe: Search the top 6 cards of
your deck for a card and draw it. Shuffle your deck. (Limit once per round.) /
__Forced__ - At the end of your turn, if you are at Dawn of the Universe: Take 1
damage and 1 physical trauma. Place 1 clue on Dawn of the Universe."

The reaction's limit is "once per round" with no "each investigator", so it is
the card's -- 'groupLimit'. "Shuffle your deck" is 'ShuffleBackIn', the search's
own return strategy.
-}
instance HasAbilities DawnOfTheUniverse_202 where
  getAbilities (DawnOfTheUniverse_202 a) =
    extendRevealed
      a
      [ groupLimit PerRound $ mkAbility a 1 $ triggered (Enters #after You (be a)) Free
      , restricted a 2 Here $ forced $ TurnEnds #when You
      ]

instance RunMessage DawnOfTheUniverse_202 where
  runMessage msg l@(DawnOfTheUniverse_202 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      search
        iid
        (attrs.ability 1)
        iid
        [(FromTopOfDeck 6, ShuffleBackIn)]
        (basic AnyCard)
        (DrawFound iid 1)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      assignDamage iid (attrs.ability 2) 1
      sufferPhysicalTrauma iid 1
      placeClues (attrs.ability 2) attrs 1
      pure l
    _ -> DawnOfTheUniverse_202 <$> liftRunMessage msg attrs
