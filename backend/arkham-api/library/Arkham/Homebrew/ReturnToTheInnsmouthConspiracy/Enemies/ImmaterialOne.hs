module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Enemies.ImmaterialOne (immaterialOne) where

import Arkham.Ability
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types (Field (EnemyLocation))
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfMaybe)
import Arkham.Helpers.Query (getLead)
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Enemies qualified as Cards
import Arkham.Location.Projection
import Arkham.Matcher
import Arkham.Projection

newtype ImmaterialOne = ImmaterialOne EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

immaterialOne :: EnemyCard ImmaterialOne
immaterialOne = enemy ImmaterialOne Cards.immaterialOne

-- | "X is the shroud value of Immaterial One's location."
instance HasModifiersFor ImmaterialOne where
  getModifiersFor (ImmaterialOne a) = modifySelfMaybe a do
    location <- MaybeT $ field EnemyLocation a.id
    shroud <- MaybeT $ field LocationShroud location
    pure [EnemyEvade shroud]

instance HasAbilities ImmaterialOne where
  getAbilities (ImmaterialOne a) =
    extend1 a
      $ restricted a 1 (thisExists a ReadyEnemy)
      $ forced
      $ EnemyWouldBeDefeated #when (be a)

{- | The designer offers an optional soft erratum shuffling it into the top ten
cards instead; printed text puts it back on top. See
return-to-innsmouth-faq-errata.md.
-}
instance RunMessage ImmaterialOne where
  runMessage msg e@(ImmaterialOne attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      cancelEnemyDefeat attrs
      lead <- getLead
      putOnTopOfDeck lead Deck.EncounterDeck attrs
      pure e
    _ -> ImmaterialOne <$> liftRunMessage msg attrs
