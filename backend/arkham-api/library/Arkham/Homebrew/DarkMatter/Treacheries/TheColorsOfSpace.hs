module Arkham.Homebrew.DarkMatter.Treacheries.TheColorsOfSpace (theColorsOfSpace) where

import Arkham.Ability
import Arkham.Card.CardDef (toCardType)
import Arkham.Card.CardType
import Arkham.Helpers.Modifiers (ModifierType (..), modifyEach)
import Arkham.Helpers.SkillTest (getSkillTest)
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Investigator.Types (Field (InvestigatorHand))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Treachery.Import.Lifted

newtype TheColorsOfSpace = TheColorsOfSpace TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theColorsOfSpace :: TreacheryCard TheColorsOfSpace
theColorsOfSpace = treachery TheColorsOfSpace Cards.theColorsOfSpace

{- | "You must commit all eligible skill cards in your hand to skill tests
performed at your location."
-}
instance HasModifiersFor TheColorsOfSpace where
  getModifiersFor (TheColorsOfSpace a) = for_ a.inThreatAreaOf \iid ->
    getSkillTest >>= traverse_ \st -> do
      colocated <- selectAny $ InvestigatorWithId st.investigator <> colocatedWith iid
      when colocated do
        cards <- fieldMap InvestigatorHand (filter ((== SkillType) . toCardType)) iid
        modifyEach a cards [MustBeCommitted]

-- | "[action] Take 1 horror: Discard The Colors of Space."
instance HasAbilities TheColorsOfSpace where
  getAbilities (TheColorsOfSpace a) =
    [restricted a 1 Here $ actionAbilityWithCost (HorrorCost (toSource a) YouTarget 1)]

instance RunMessage TheColorsOfSpace where
  runMessage msg t@(TheColorsOfSpace attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> TheColorsOfSpace <$> liftRunMessage msg attrs
