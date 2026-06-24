module Arkham.Agenda.Cards.SchemesInTheDarkBeyond (schemesInTheDarkBeyond) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.GuardiansOfTheAbyss.Helpers
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher hiding (InvestigatorDefeated)
import Arkham.Trait (Trait (Brotherhood))

newtype SchemesInTheDarkBeyond = SchemesInTheDarkBeyond AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

schemesInTheDarkBeyond :: AgendaCard SchemesInTheDarkBeyond
schemesInTheDarkBeyond = agenda (2, A) SchemesInTheDarkBeyond Cards.schemesInTheDarkBeyond (Static 12)

instance HasModifiersFor SchemesInTheDarkBeyond where
  getModifiersFor (SchemesInTheDarkBeyond a) = do
    modifySelect a Anyone [CannotParleyWith $ EnemyWithTrait Brotherhood]
    modifySelect a (EnemyWithTrait Brotherhood) [CannotBeFlipped]

instance HasAbilities SchemesInTheDarkBeyond where
  getAbilities (SchemesInTheDarkBeyond a) =
    [ restricted a 1 criteria
        $ forced
        $ PlacedDoomCounter #after AnySource (TargetIs $ toTarget a)
    ]
   where
    criteria = if a.doom == 4 || a.doom == 8 then NoRestriction else Never

instance RunMessage SchemesInTheDarkBeyond where
  runMessage msg a@(SchemesInTheDarkBeyond attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      addStrengthOfTheAbyss 1
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      selectEach UneliminatedInvestigator \iid -> do
        investigatorTakenByTheAbyss iid
        push $ InvestigatorDefeated (toSource attrs) iid
      push R1
      pure a
    _ -> SchemesInTheDarkBeyond <$> liftRunMessage msg attrs
