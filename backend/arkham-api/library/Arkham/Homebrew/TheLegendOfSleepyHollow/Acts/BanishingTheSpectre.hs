module Arkham.Homebrew.TheLegendOfSleepyHollow.Acts.BanishingTheSpectre (
  banishingTheSpectre,
) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Fight qualified as Fight
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.TheLegendOfSleepyHollow.Enemies.TheHeadlessHorseman (
  ignoreTheHorsemansForcedEffect,
 )
import Arkham.Homebrew.TheLegendOfSleepyHollow.Helpers
import Arkham.I18n
import Arkham.Investigator.Types (Field (InvestigatorClues))
import Arkham.Matcher
import Arkham.Projection

newtype BanishingTheSpectre = BanishingTheSpectre ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

banishingTheSpectre :: ActCard BanishingTheSpectre
banishingTheSpectre = act (3, A) BanishingTheSpectre Cards.banishingTheSpectre Nothing

{- | "The Headless Horseman gets +4[per_investigator] health and cannot leave
play except by being defeated. Ignore the forced effect on The Headless
Horseman."
-}
instance HasModifiersFor BanishingTheSpectre where
  getModifiersFor (BanishingTheSpectre a) = when (onSide A a) do
    extraHealth <- perPlayer 4
    modifySelect
      a
      headlessHorseman
      [HealthModifier extraHealth, CannotLeavePlay, ignoreTheHorsemansForcedEffect]

instance HasAbilities BanishingTheSpectre where
  getAbilities = actAbilities \a ->
    [ scenarioI18n
        $ withI18nTooltip "banishingTheSpectre.banish"
        $ skillTestAbility
        $ restricted
          a
          1
          ( youExist (InvestigatorWithClues $ atLeast 1)
              <> exists (CanFightEnemy (a.ability 1) <> headlessHorseman)
          )
        $ fightActionWith_ #intellect
    , onlyOnce
        $ mkAbility a 2
        $ Objective
        $ forced (EnemyDefeated #after Anyone ByAny headlessHorseman)
    ]

instance RunMessage BanishingTheSpectre where
  runMessage msg a@(BanishingTheSpectre attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      clues <- field InvestigatorClues iid
      scenarioI18n
        $ scope "banishingTheSpectre"
        $ countVar (min 3 clues)
        $ chooseAmount iid "cluesToSpend" "$clues" 1 (min 3 clues) attrs
      pure a
    ResolveAmounts iid (getChoiceAmount "$clues" -> spent) (isTarget attrs -> True) -> do
      spendClues iid spent
      sid <- getRandom
      skillTestModifiers sid (attrs.ability 1) iid [SkillModifier #intellect spent, DamageDealt spent]
      chooseFightEnemyMatchEdit
        sid
        iid
        (attrs.ability 1)
        headlessHorseman
        (Fight.withSkillType #intellect)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advanceVia #other attrs attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R2
      pure a
    _ -> BanishingTheSpectre <$> liftRunMessage msg attrs
