module Arkham.Homebrew.DarkMatter.Treacheries.IrresistibleTruths (irresistibleTruths) where

import Arkham.Card.CardDef (toCardType)
import Arkham.Card.CardType
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.DarkMatter.Traits (pattern Carcosa)
import Arkham.Investigator.Types (Field (InvestigatorHand))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Treachery.Import.Lifted

newtype IrresistibleTruths = IrresistibleTruths TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

irresistibleTruths :: TreacheryCard IrresistibleTruths
irresistibleTruths = treachery IrresistibleTruths Cards.irresistibleTruths

{- | "Revelation - Test [intellect] (3). If you are at a [[Carcosa]] location, you
must commit all eligible skill cards to this test. If you fail, take 2 horror."
-}
instance RunMessage IrresistibleTruths where
  runMessage msg t@(IrresistibleTruths attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      inCarcosa <- selectAny $ locationWithInvestigator iid <> LocationWithTrait Carcosa
      when inCarcosa do
        cards <- fieldMap InvestigatorHand (filter ((== SkillType) . toCardType)) iid
        for_ cards \card -> skillTestModifier sid attrs card MustBeCommitted
      revelationSkillTest sid iid attrs #intellect (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignHorror iid attrs 2
      pure t
    _ -> IrresistibleTruths <$> liftRunMessage msg attrs
