module Arkham.Homebrew.DarkMatter.Treacheries.FromTheDark (fromTheDark) where

import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (campaignI18n, getScanningDeck)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype FromTheDark = FromTheDark TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fromTheDark :: TreacheryCard FromTheDark
fromTheDark = treachery FromTheDark Cards.fromTheDark

{- | "Revelation - Test [agility] (6). Reduce the difficulty of this test by 1 for
each card in the scanning deck. If you fail, either take 1 damage for each point
you fail by, or search the encounter deck and discard pile for a Mimic enemy and
draw it."
-}
instance RunMessage FromTheDark where
  runMessage msg t@(FromTheDark attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      scanning <- length <$> getScanningDeck
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed $ max 0 (6 - scanning))
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      chooseOneM iid $ campaignI18n do
        countVar n $ labeled "fromTheDark.takeDamage" $ assignDamage iid attrs n
        labeled "fromTheDark.drawAMimic" $ findAndDrawEncounterCard iid (cardIs Enemies.mimic)
      pure t
    _ -> FromTheDark <$> liftRunMessage msg attrs
