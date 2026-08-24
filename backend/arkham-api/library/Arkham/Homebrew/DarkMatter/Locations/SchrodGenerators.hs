module Arkham.Homebrew.DarkMatter.Locations.SchrodGenerators (schrodGenerators) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (drawRandomFacedownCard, yourFacedownCardsAtLeast)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype SchrodGenerators = SchrodGenerators LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

schrodGenerators :: LocationCard SchrodGenerators
schrodGenerators = symbolLabel $ location SchrodGenerators Cards.schrodGenerators 2 (Static 0)

-- "Investigators at this location cannot cancel or ignore card effects or game effects."
instance HasModifiersFor SchrodGenerators where
  getModifiersFor (SchrodGenerators a) =
    modifySelect
      a
      (InvestigatorAt $ be a)
      [CannotCancelCardOrGameEffects, CannotIgnoreCardOrGameEffects]

{- | "[free] If you have 4 or more face-down encounter cards in your threat area,
draw 1 of them: Gain 2 clues from the token bank. (Limit once per round.)"

The card count is the ability's activation condition, so it is a 'Criterion':
checking it only in the handler left a free ability on offer that did nothing.
-}
instance HasAbilities SchrodGenerators where
  getAbilities (SchrodGenerators a) =
    extendRevealed1 a
      $ playerLimit PerRound
      $ restricted a 1 (Here <> yourFacedownCardsAtLeast 4)
      $ FastAbility Free

instance RunMessage SchrodGenerators where
  runMessage msg l@(SchrodGenerators attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      void $ drawRandomFacedownCard iid
      gainClues iid (attrs.ability 1) 2
      pure l
    _ -> SchrodGenerators <$> liftRunMessage msg attrs
