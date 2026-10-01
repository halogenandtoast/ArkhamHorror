module Arkham.Homebrew.CircusExMortis.Treacheries.HungerOfThousands (hungerOfThousands) where

import Arkham.Enemy.Types (Field (EnemyDamage, EnemyHealth))
import Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (scenarioI18n)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Treachery.Import.Lifted

newtype HungerOfThousands = HungerOfThousands TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hungerOfThousands :: TreacheryCard HungerOfThousands
hungerOfThousands = treachery HungerOfThousands Cards.hungerOfThousands

instance RunMessage HungerOfThousands where
  runMessage msg t@(HungerOfThousands attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      {- By title, not `enemyIs`: the two sides of Ravenous Brood are separate card
      codes, so a copy that has already been flipped would stop matching its own
      front's code while still being a ready Ravenous Brood the card can pick. -}
      broods <- select $ FarthestEnemyFromAll $ EnemyWithTitle "Ravenous Brood" <> ReadyEnemy
      -- No ready copy in play is no game state to change: nothing is asked and the
      -- treachery just goes to the discard pile.
      unless (null broods) do
        -- The card names one copy; ties are the drawing investigator's to break.
        scenarioI18n "thousandToOne" $ scope "hungerOfThousands" $ chooseOrRunOneM iid do
          questionLabeled "chooseBrood"
          targets broods \brood -> do
            flipOverBy iid attrs brood
            push $ ForTarget (toTarget brood) (DoStep 1 msg)
      pure t
    ForTarget (EnemyTarget brood) (DoStep 1 (Revelation _ (isSource attrs -> True))) -> do
      {- The flip has already swapped the sides. `ReplaceEnemy … Swap` carries the
      damage over and never checks defeat, so "if flipping would defeat it, instead
      heal it until it has 1 health remaining" is read here, against the side the
      copy now shows -- the only point where that side's health is knowable. -}
      mhealth <- field EnemyHealth brood
      damage <- field EnemyDamage brood
      for_ mhealth \health -> when (damage >= health) $ healDamage brood attrs (damage - health + 1)
      resolveEnemyPhaseOf brood
      pure t
    _ -> HungerOfThousands <$> liftRunMessage msg attrs
