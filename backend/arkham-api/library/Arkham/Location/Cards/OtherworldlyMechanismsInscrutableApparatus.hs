module Arkham.Location.Cards.OtherworldlyMechanismsInscrutableApparatus (otherworldlyMechanismsInscrutableApparatus) where

import Arkham.Ability
import Arkham.Card (cardIsWeakness, toCard)
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Investigator.Projection ()
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.TheGrandVault.Helpers
import Arkham.Trait (Trait (Glyph))

newtype OtherworldlyMechanismsInscrutableApparatus = OtherworldlyMechanismsInscrutableApparatus LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

otherworldlyMechanismsInscrutableApparatus :: LocationCard OtherworldlyMechanismsInscrutableApparatus
otherworldlyMechanismsInscrutableApparatus = location OtherworldlyMechanismsInscrutableApparatus Cards.otherworldlyMechanismsInscrutableApparatus 4 (Static 1)

instance HasAbilities OtherworldlyMechanismsInscrutableApparatus where
  getAbilities (OtherworldlyMechanismsInscrutableApparatus a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ RevealLocation #when You (be a)
      , restricted a 2 Here actionAbility
      ]

instance RunMessage OtherworldlyMechanismsInscrutableApparatus where
  runMessage msg l@(OtherworldlyMechanismsInscrutableApparatus attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      glyphs <- getSetAsideCardsMatching (CardWithTrait Glyph <> #treachery)
      for_ (nonEmpty glyphs) $ sample >=> drawCard iid
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      -- Reveal the top 3 cards of your deck: draw each revealed weakness and
      -- remove the rest from the game.
      cards <- map toCard <$> iid.topOfDeckN 3
      for_ cards revealCard
      let (weaknesses, rest) = partition cardIsWeakness cards
      for_ weaknesses (drawCard iid)
      for_ rest removeCardFromGame
      activateLocation (attrs.ability 2) attrs.id
      pure l
    _ -> OtherworldlyMechanismsInscrutableApparatus <$> liftRunMessage msg attrs
