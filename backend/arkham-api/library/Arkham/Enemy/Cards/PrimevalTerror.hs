module Arkham.Enemy.Cards.PrimevalTerror (primevalTerror) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Matcher
import Arkham.Scenarios.ObsidianCanyons.Helpers

newtype PrimevalTerror = PrimevalTerror EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

primevalTerror :: EnemyCard PrimevalTerror
primevalTerror = enemy PrimevalTerror Cards.primevalTerror

instance HasModifiersFor PrimevalTerror where
  getModifiersFor (PrimevalTerror a) = do
    modifySelf a [CannotMakeAttacksOfOpportunity]
    modifySelect a (InvestigatorEngagedWith (be a)) [canEnterOpenSky]

instance HasAbilities PrimevalTerror where
  getAbilities (PrimevalTerror a) =
    extend
      a
      [ mkAbility a 1 $ SilentForcedAbility $ EnemyDefeated #when Anyone ByAny (be a)
      , mkAbility a 2
          $ SilentForcedAbility
          $ EnemyDisengaged #after (InvestigatorAt isOpenSky) (be a)
      ]

instance RunMessage PrimevalTerror where
  runMessage msg e@(PrimevalTerror attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      -- The investigator who defeats Primeval Terror need not be the one it is
      -- holding over open sky. Resolve this in the #when window, while its
      -- engaged investigator can still be identified.
      selectEach (InvestigatorEngagedWith (be attrs) <> InvestigatorAt isOpenSky) \iid -> do
        sufferPhysicalTrauma iid 1
        investigatorDefeated (attrs.ability 1) iid
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sufferPhysicalTrauma iid 1
      investigatorDefeated (attrs.ability 2) iid
      pure e
    _ -> PrimevalTerror <$> liftRunMessage msg attrs
