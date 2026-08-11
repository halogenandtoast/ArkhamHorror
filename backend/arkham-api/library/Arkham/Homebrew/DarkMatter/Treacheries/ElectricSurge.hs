module Arkham.Homebrew.DarkMatter.Treacheries.ElectricSurge (electricSurge) where

import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.DarkMatter.Traits (pattern AI)
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype ElectricSurge = ElectricSurge TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

electricSurge :: TreacheryCard ElectricSurge
electricSurge = treachery ElectricSurge Cards.electricSurge

{- | "Revelation - Test [agility] (2). This test gains +1 difficulty for each
[[AI]] encounter card in your threat area. If you fail, each investigator at
your location takes 1 damage, and Electric Surge gains surge."
-}
instance RunMessage ElectricSurge where
  runMessage msg t@(ElectricSurge attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      n <- selectCount $ TreacheryWithTrait AI <> TreacheryInThreatAreaOf (InvestigatorWithId iid)
      sid <- getRandom
      when (n > 0) $ skillTestModifier sid attrs sid (Difficulty n)
      revelationSkillTest sid iid attrs #agility (Fixed 2)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      iids <- select $ InvestigatorAt $ locationWithInvestigator iid
      for_ iids \iid' -> assignDamage iid' attrs 1
      gainSurge attrs
      pure t
    _ -> ElectricSurge <$> liftRunMessage msg attrs
