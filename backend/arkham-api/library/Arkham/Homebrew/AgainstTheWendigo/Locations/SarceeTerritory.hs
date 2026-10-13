module Arkham.Homebrew.AgainstTheWendigo.Locations.SarceeTerritory (sarceeTerritory) where

import Arkham.Ability
import Arkham.Helpers.Cost (getSpendableResources)
import Arkham.Helpers.Modifiers (ModifierType (Difficulty))
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Helpers (campaignI18n, civilizedResign)
import Arkham.Homebrew.AgainstTheWendigo.Key
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log (record)

newtype SarceeTerritory = SarceeTerritory LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sarceeTerritory :: LocationCard SarceeTerritory
sarceeTerritory = location SarceeTerritory Cards.sarceeTerritory 3 (PerPlayer 1)

{- | "An investigator may attempt one of the following 2 actions (Collective
limit of once per game)": persuade Imala, or intimidate her. Both are Parleys and
both reveal the Isolated Land on a success; only the second records that the
Sarcee are hunting you.

The limit spans the pair -- attempting either takes both off the card, not one
use of each -- which no 'AbilityLimit' says, so the attempt is remembered on the
location instead. Set when the parley is attempted rather than when it resolves,
because attempting is what spends the chance.
-}
instance HasAbilities SarceeTerritory where
  getAbilities (SarceeTerritory a) =
    extendRevealed a $ civilizedResign a : if attempted then [] else [persuade, intimidate]
   where
    attempted = toResultDefault False a.meta
    persuade =
      campaignI18n
        $ withI18nTooltip "sarceeTerritory.persuade"
        $ skillTestAbility
        $ restricted a 1 Here parleyAction_
    intimidate =
      campaignI18n
        $ withI18nTooltip "sarceeTerritory.intimidate"
        $ skillTestAbility
        $ restricted a 2 Here parleyAction_

instance RunMessage SarceeTerritory where
  runMessage msg l@(SarceeTerritory attrs) = runQueueT $ case msg of
    {- "You can spend up to 5 resources to reduce the difficulty of this test by
    as many points."

    Asked before the parley rather than during it, so the reduction is on the
    test from the start -- and skipped outright when there is nothing to spend,
    which would otherwise put an empty dialog on screen. -}
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      most <- min 5 <$> getSpendableResources iid
      if most == 0
        then persuade iid 0
        else
          chooseAmounts
            iid
            "$againstTheWendigo.sarceeTerritory.spendResources"
            (MaxAmountTarget most)
            [("Resources", (0, most))]
            attrs
      pure $ SarceeTerritory $ setMeta True attrs
    ResolveAmounts iid (getChoiceAmount "Resources" -> n) (isTarget attrs -> True) -> do
      persuade iid n
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      record TheSarceeAreHuntingYouDown
      parley sid iid (attrs.ability 2) attrs #combat (Fixed 3)
      pure $ SarceeTerritory $ setMeta True attrs
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> revealIsolatedLand l
    PassedThisSkillTest _ (isAbilitySource attrs 2 -> True) -> revealIsolatedLand l
    _ -> SarceeTerritory <$> liftRunMessage msg attrs
   where
    persuade iid n = do
      sid <- getRandom
      when (n > 0) do
        spendResources iid n
        skillTestModifier sid (attrs.ability 1) sid (Difficulty (-n))
      parley sid iid (attrs.ability 1) attrs #intellect (Fixed 6)
    -- "If you succeed, reveal the Isolated Land."
    revealIsolatedLand l' = do
      selectForMaybeM (locationIs Cards.hiddenHut) reveal
      pure l'
