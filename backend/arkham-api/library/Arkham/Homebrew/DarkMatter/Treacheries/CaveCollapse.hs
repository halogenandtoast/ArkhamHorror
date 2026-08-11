module Arkham.Homebrew.DarkMatter.Treacheries.CaveCollapse (caveCollapse) where

import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Trait (Trait (Cave))
import Arkham.Treachery.Import.Lifted

newtype CaveCollapse = CaveCollapse TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

caveCollapse :: TreacheryCard CaveCollapse
caveCollapse = treachery CaveCollapse Cards.caveCollapse

{- | "Revelation - Each investigator at a [[Cave]] location must test [agility]
(3). Each investigator who fails takes 1 damage and loses 1 action."
-}
instance RunMessage CaveCollapse where
  runMessage msg t@(CaveCollapse attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      investigators <- select $ InvestigatorAt $ LocationWithTrait Cave
      for_ investigators \iid -> forInvestigator iid msg
      pure t
    ForInvestigator iid (Revelation _ (isSource attrs -> True)) -> do
      sid <- getRandom
      beginSkillTest sid iid attrs iid #agility (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignDamage iid attrs 1
      loseActions iid attrs 1
      pure t
    _ -> CaveCollapse <$> liftRunMessage msg attrs
