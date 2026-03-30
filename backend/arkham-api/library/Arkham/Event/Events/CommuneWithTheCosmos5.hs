module Arkham.Event.Events.CommuneWithTheCosmos5 (communeWithTheCosmos5) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Investigator.Types (Field (..))
import Arkham.Modifier
import Arkham.Projection

newtype CommuneWithTheCosmos5 = CommuneWithTheCosmos5 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

communeWithTheCosmos5 :: EventCard CommuneWithTheCosmos5
communeWithTheCosmos5 = event CommuneWithTheCosmos5 Cards.communeWithTheCosmos5

instance RunMessage CommuneWithTheCosmos5 where
  runMessage msg e@(CommuneWithTheCosmos5 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      chooseAmount iid "Horror" "Horror" 0 3 attrs
      pure e
    ResolveAmounts iid (getChoiceAmount "Horror" -> n) (isTarget attrs -> True) -> do
      when (n > 0) $ assignHorror iid attrs n
      sid <- getRandom
      skillTestModifier sid attrs iid (SkillModifier #intellect 2)
      investigate sid iid attrs
      pure e
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      horror <- field InvestigatorHorror iid
      when (horror > 0) $ discoverAtYourLocation IsInvestigate iid attrs (min horror 4)
      pure e
    _ -> CommuneWithTheCosmos5 <$> liftRunMessage msg attrs
