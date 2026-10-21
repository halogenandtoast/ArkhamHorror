module Arkham.Treachery.Cards.TracesToNowhere.TheMysteryDeepens (theMysteryDeepens) where

import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (CrimeScene))
import Arkham.Treachery.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TheMysteryDeepens = TheMysteryDeepens TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theMysteryDeepens :: TreacheryCard TheMysteryDeepens
theMysteryDeepens = treachery TheMysteryDeepens Cards.theMysteryDeepens

instance RunMessage TheMysteryDeepens where
  runMessage msg t@(TheMysteryDeepens attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #intellect (Fixed 4)
      pure t
    FailedThisSkillTestBy _ (isSource attrs -> True) n -> do
      doStep n msg
      pure t
    DoStep n msg'@(FailedThisSkillTestBy iid (isSource attrs -> True) total) | n > 0 -> do
      destinations <-
        select
          $ RevealedLocation
          <> LocationWithTrait CrimeScene
          <> not_ (LocationWithInvestigator Anyone)
      source <- select $ locationWithInvestigator iid <> LocationWithAnyClues
      case (destinations, source) of
        (_ : _, [from]) -> do
          chooseTargetM iid destinations \dest -> moveTokensNoDefeated attrs from dest #clue 1
          doStep (n - 1) msg'
        _ -> when (n == total) $ assignHorror iid attrs 2
      pure t
    _ -> TheMysteryDeepens <$> liftRunMessage msg attrs
