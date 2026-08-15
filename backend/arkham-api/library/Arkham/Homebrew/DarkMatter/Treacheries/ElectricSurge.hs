module Arkham.Homebrew.DarkMatter.Treacheries.ElectricSurge (electricSurge) where

import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.DarkMatter.Traits (pattern AI)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Import.Lifted

newtype ElectricSurge = ElectricSurge TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

electricSurge :: TreacheryCard ElectricSurge
electricSurge = treachery ElectricSurge Cards.electricSurge

instance RunMessage ElectricSurge where
  runMessage msg t@(ElectricSurge attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility
        $ SumCalculation
          [ Fixed 2
          , CountTreacheries $ TreacheryWithTrait AI <> TreacheryInThreatAreaOf (InvestigatorWithId iid)
          , CountEnemies $ EnemyWithTrait AI <> EnemyWithPlacement (InThreatArea iid)
          ]
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      selectEach (InvestigatorAt $ locationWithInvestigator iid) $ assignDamageTo attrs 1
      gainSurge attrs
      pure t
    _ -> ElectricSurge <$> liftRunMessage msg attrs
