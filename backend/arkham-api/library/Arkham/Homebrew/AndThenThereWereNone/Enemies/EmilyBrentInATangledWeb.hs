{- | The Spinster, unmasked. Every enemy phase she summons the table to her or
takes an offering off the board -- and if the board runs low, she wakes up.

"each investigator must either move directly to Emily Brent's location or
remove 1 offering...": an investigator already standing with her has no move to
make, and an option you cannot perform is not one you may choose, so the
offering is the only branch left to them. (@docs/homebrew.md@: standard rules
govern where the fan wording is loose.) With no unchosen copy left either,
nothing happens.
-}
module Arkham.Homebrew.AndThenThereWereNone.Enemies.EmilyBrentInATangledWeb (
  emilyBrentInATangledWeb,
) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.AndThenThereWereNone.Helpers
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (moveTo)

newtype EmilyBrentInATangledWeb = EmilyBrentInATangledWeb EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

emilyBrentInATangledWeb :: EnemyCard EmilyBrentInATangledWeb
emilyBrentInATangledWeb = enemy EmilyBrentInATangledWeb Cards.emilyBrentInATangledWeb

instance HasAbilities EmilyBrentInATangledWeb where
  getAbilities (EmilyBrentInATangledWeb a) =
    extend1 a
      $ scenarioI18n
      $ withI18nTooltip "emilyBrentInATangledWeb.web"
      $ mkAbility a 1
      $ forced
      $ PhaseBegins #when #enemy

instance RunMessage EmilyBrentInATangledWeb where
  runMessage msg e@(EmilyBrentInATangledWeb attrs) = runQueueT $ case msg of
    -- "In player order, each investigator must either..." Each seat is handed
    -- its own message so its choices are built after the previous seat paid.
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      eachInvestigator (`forInvestigator` msg)
      doStep 1 msg
      pure e
    ForInvestigator iid (UseThisAbility _ (isSource attrs -> True) 1) -> do
      withEmily <- iid <=~> InvestigatorAt (locationWithEnemy attrs)
      mlid <- if withEmily then pure Nothing else selectOne (locationWithEnemy attrs)
      candidates <- unchosenSigils
      when (isJust mlid || notNull candidates) do
        chooseOneM iid $ scenarioI18n $ scope "emilyBrentInATangledWeb" do
          for_ mlid $ labeled "moveToEmilyBrent" . moveTo (attrs.ability 1) iid
          when (notNull candidates)
            $ labeled "removeOffering"
            $ chooseOrRunOneM iid
            $ targets candidates (removeOfferingFromSigil (attrs.ability 1))
      pure e
    -- "Then, if there are 1{perPlayer} or fewer offerings among all copies,
    -- ready Emily Brent."
    DoStep 1 (UseThisAbility _ (isSource attrs -> True) 1) -> do
      total <- totalOfferings
      threshold <- perPlayer 1
      when (total <= threshold) $ readyThis attrs
      pure e
    _ -> EmilyBrentInATangledWeb <$> liftRunMessage msg attrs
