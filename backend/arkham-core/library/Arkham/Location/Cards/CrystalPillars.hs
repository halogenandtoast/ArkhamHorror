module Arkham.Location.Cards.CrystalPillars
  ( crystalPillars
  , CrystalPillars(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype CrystalPillars = CrystalPillars LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crystalPillars :: LocationCard CrystalPillars
crystalPillars = location CrystalPillars Cards.crystalPillars 1 (PerPlayer 2)

instance HasAbilities CrystalPillars where
  getAbilities (CrystalPillars attrs) = withBaseAbilities
    attrs
    [ mkAbility attrs 1
      $ ForcedAbility
      $ Enters Timing.After You
      $ LocationWithId
      $ toId attrs
    ]

instance RunMessage CrystalPillars where
  runMessage msg l@(CrystalPillars attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      n <- getVengeanceInVictoryDisplay
      push $ beginSkillTest
        iid
        (toSource attrs)
        (InvestigatorTarget iid)
        Nothing
        SkillWillpower
        (1 + n)
      pure l
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget{} _ _
      -> do
        push $ InvestigatorAssignDamage iid (toSource attrs) DamageAny 1 1
        pure l
    _ -> CrystalPillars <$> runMessage msg attrs
