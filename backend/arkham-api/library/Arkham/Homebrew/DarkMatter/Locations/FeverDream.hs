module Arkham.Homebrew.DarkMatter.Locations.FeverDream (feverDream) where

import Arkham.Card.CardDef (toCardType)
import Arkham.Card.CardType
import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modifyEach)
import Arkham.Helpers.SkillTest (getSkillTest)
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.DarkMatter.Key
import Arkham.Investigator.Types (Field (InvestigatorHand))
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Log
import Arkham.Projection

newtype FeverDream = FeverDream LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

feverDream :: LocationCard FeverDream
feverDream = symbolLabel $ location FeverDream Cards.feverDream 3 (PerPlayer 2)

{- | "As an additional cost to investigate Fever Dream, commit all eligible skill
cards in your hand to that investigation." Every skill card in the
investigator's hand is marked 'MustBeCommitted'; the engine narrows that to the
ones actually committable.
-}
instance HasModifiersFor FeverDream where
  getModifiersFor (FeverDream a) = do
    getSkillTest >>= traverse_ \st ->
      when (isTarget a st.target && st.action == Just #investigate) do
        cards <- fieldMap InvestigatorHand (filter ((== SkillType) . toCardType)) st.investigator
        modifyEach a cards [MustBeCommitted]

instance RunMessage FeverDream where
  runMessage msg l@(FeverDream attrs) = runQueueT $ case msg of
    -- "Forced - When Fever Dream is added to the victory display: Record in your
    -- Campaign Log that you have witnessed the unconscious pandemonium."
    AddToVictory _ (isTarget attrs -> True) -> do
      record YouHaveWitnessedTheUnconsciousPandemonium
      pure l
    _ -> FeverDream <$> liftRunMessage msg attrs
