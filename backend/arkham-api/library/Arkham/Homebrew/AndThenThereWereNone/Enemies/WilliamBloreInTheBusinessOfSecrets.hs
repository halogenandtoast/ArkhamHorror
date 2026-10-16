{- | The Inspector, unmasked. Hitting him either throws you across the island or
costs an offering, and an uneven board costs you your nerve.
-}
module Arkham.Homebrew.AndThenThereWereNone.Enemies.WilliamBloreInTheBusinessOfSecrets (
  williamBloreInTheBusinessOfSecrets,
) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.AndThenThereWereNone.Helpers
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (moveTo)

newtype WilliamBloreInTheBusinessOfSecrets = WilliamBloreInTheBusinessOfSecrets EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

williamBloreInTheBusinessOfSecrets :: EnemyCard WilliamBloreInTheBusinessOfSecrets
williamBloreInTheBusinessOfSecrets =
  enemy WilliamBloreInTheBusinessOfSecrets Cards.williamBloreInTheBusinessOfSecrets

instance HasAbilities WilliamBloreInTheBusinessOfSecrets where
  getAbilities (WilliamBloreInTheBusinessOfSecrets a) =
    extend1 a
      $ scenarioI18n
      $ withI18nTooltip "williamBloreInTheBusinessOfSecrets.secrets"
      $ mkAbility a 1
      $ forced
      $ EnemyDealtDamage #after AnyDamageEffect (be a) (SourceUsedBy You)

instance RunMessage WilliamBloreInTheBusinessOfSecrets where
  runMessage msg e@(WilliamBloreInTheBusinessOfSecrets attrs) = runQueueT $ case msg of
    -- "You must either move directly to the furthest location from you or spend
    -- 1 offering from a copy of Antediluvian Sigils."
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      furthest <- select $ FarthestLocationFromInvestigator (InvestigatorWithId iid) Anywhere
      sigils <- select $ antediluvianSigils <> AssetWithSpendableUses (atLeast 1) #offering
      chooseOneM iid $ scenarioI18n $ scope "williamBloreInTheBusinessOfSecrets" do
        when (notNull furthest)
          $ labeled "moveToFurthest"
          $ chooseOrRunOneM iid
          $ targets furthest (moveTo (attrs.ability 1) iid)
        when (notNull sigils)
          $ labeled "spendOffering"
          $ chooseOrRunOneM iid
          $ targets sigils \aid -> spendUses (attrs.ability 1) aid #offering 1
      doStep 1 msg
      pure e
    -- "Then, if any copy of Antediluvian Sigils has 2 or more offerings than any
    -- other copy, take 1 horror."
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      offerings <- offeringsPerSigil
      when (notNull offerings && maximumEx offerings - minimumEx offerings >= 2)
        $ assignHorror iid (attrs.ability 1) 1
      pure e
    _ -> WilliamBloreInTheBusinessOfSecrets <$> liftRunMessage msg attrs
