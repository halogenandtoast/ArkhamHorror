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
    [ mkAbility a 1
        $ forced
        $ PlacedDoomCounter #after AnySource (TargetIs $ toTarget a)
    ]

instance RunMessage SchemesInTheDarkBeyond where
  runMessage msg a@(SchemesInTheDarkBeyond attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      when (attrs.doom == 4 || attrs.doom == 8) $ addStrengthOfTheAbyss 1
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      selectEach UneliminatedInvestigator \iid -> do
        push $ InvestigatorDefeated (toSource attrs) iid
        investigatorTakenByTheAbyss iid
      push R1
      pure a
    _ -> SchemesInTheDarkBeyond <$> liftRunMessage msg attrs
