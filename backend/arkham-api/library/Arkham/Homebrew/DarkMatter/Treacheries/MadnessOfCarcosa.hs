module Arkham.Homebrew.DarkMatter.Treacheries.MadnessOfCarcosa (madnessOfCarcosa) where

import Arkham.Ability
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher hiding (InvestigatorDefeated)
import Arkham.Matcher qualified as Matcher
import Arkham.Placement
import Arkham.Treachery.Import.Lifted

newtype MadnessOfCarcosa = MadnessOfCarcosa TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

madnessOfCarcosa :: TreacheryCard MadnessOfCarcosa
madnessOfCarcosa = treachery MadnessOfCarcosa Cards.madnessOfCarcosa

{- | "Surge. Peril. Hidden. Revelation - Secretly add this card to your hand.
[reaction] After an investigator is defeated: Add this card to the victory
display."
-}
instance HasAbilities MadnessOfCarcosa where
  getAbilities (MadnessOfCarcosa a) = case a.placement of
    HiddenInHand iid ->
      [ restricted a 1 (youExist $ InvestigatorWithId iid)
          $ freeReaction
          $ Matcher.InvestigatorDefeated #after ByAny Anyone
      ]
    _ -> []

instance RunMessage MadnessOfCarcosa where
  runMessage msg t@(MadnessOfCarcosa attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      addHiddenToHand iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      addToVictory iid attrs
      pure t
    _ -> MadnessOfCarcosa <$> liftRunMessage msg attrs
