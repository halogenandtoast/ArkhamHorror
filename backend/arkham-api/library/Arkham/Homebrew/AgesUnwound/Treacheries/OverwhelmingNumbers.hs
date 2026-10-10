module Arkham.Homebrew.AgesUnwound.Treacheries.OverwhelmingNumbers (overwhelmingNumbers) where

import Arkham.Helpers.Investigator (canPlaceCluesOnYourLocation)
import Arkham.Helpers.Modifiers (ModifierType (Difficulty))
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.Unstuck.Helpers (unstuckI18n)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype OverwhelmingNumbers = OverwhelmingNumbers TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

overwhelmingNumbers :: TreacheryCard OverwhelmingNumbers
overwhelmingNumbers = treachery OverwhelmingNumbers Cards.overwhelmingNumbers

{- | "Revelation - Each investigator at your location tests [combat] or [agility]
(4). An investigator at your location may place one of their clues on your
location to reduce the difficulty of each test by 2. Each investigator that
fails takes 1 damage and 1 horror."

The clue is offered once, before any test is made, because it reduces /each/
test -- so the reduction is a modifier on every test this card creates rather
than on one of them.
-}
instance RunMessage OverwhelmingNumbers where
  runMessage msg t@(OverwhelmingNumbers attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      here <- select $ colocatedWith iid
      payers <- filterM canPlaceCluesOnYourLocation here
      chooseOneM iid $ unstuckI18n $ scope "overwhelmingNumbers" do
        labeled "doNotPlaceAClue" $ doStep 1 msg
        for_ payers \payer ->
          targeting payer do
            push $ InvestigatorPlaceCluesOnLocation payer (toSource attrs) 1
            doStep 2 msg
      pure t
    DoStep n (Revelation iid (isSource attrs -> True)) | n `elem` [1, 2] -> do
      here <- select $ colocatedWith iid
      for_ here \i -> do
        sid <- getRandom
        when (n == 2) $ skillTestModifier sid attrs sid (Difficulty (-2))
        chooseOneM i $ unstuckI18n $ scope "overwhelmingNumbers" do
          labeled "testCombat" $ beginSkillTest sid i attrs i #combat (Fixed 4)
          labeled "testAgility" $ beginSkillTest sid i attrs i #agility (Fixed 4)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignDamageAndHorror iid attrs 1 1
      pure t
    _ -> OverwhelmingNumbers <$> liftRunMessage msg attrs
