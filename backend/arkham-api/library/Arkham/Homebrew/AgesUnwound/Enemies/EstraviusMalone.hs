module Arkham.Homebrew.AgesUnwound.Enemies.EstraviusMalone (estraviusMalone) where

import Arkham.Ability
import Arkham.Card
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.Modifiers (ModifierType (SkillModifier), modifySelect)
import Arkham.Helpers.Scenario (getEncounterDiscard)
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Cards
import Arkham.Matcher
import Arkham.Scenario.Deck (ScenarioEncounterDeckKey (RegularEncounterDeck))
import Arkham.Trait (Trait (Paradox, Power))

newtype EstraviusMalone = EstraviusMalone EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | /Dread Sorcerer of Yog-Sothoth/. Hunter and Elite are on the card def.
estraviusMalone :: EnemyCard EstraviusMalone
estraviusMalone = enemy EstraviusMalone Cards.estraviusMalone

{- | "While Estravius Malone is ready, investigators at his location get -2
[willpower]."
-}
instance HasModifiersFor EstraviusMalone where
  getModifiersFor (EstraviusMalone a) =
    unless a.exhausted
      $ modifySelect a (InvestigatorAt $ locationWithEnemy a) [SkillModifier #willpower (-2)]

{- | "Forced - When Estravius Malone attacks an investigator: That investigator
draws the topmost [[Power]] or [[Paradox]] treachery in the encounter discard
pile."

The window carries @You@ rather than @Anyone@ so the Forced is offered to exactly
the investigator being attacked -- "that investigator" -- instead of to every
seat (see @project_forced_window_who_must_be_you@).
-}
instance HasAbilities EstraviusMalone where
  getAbilities (EstraviusMalone a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyAttacks #when You AnyEnemyAttack (be a)

-- | "the topmost [[Power]] or [[Paradox]] treachery in the encounter discard pile"
topmostParadoxOrPower :: CardMatcher
topmostParadoxOrPower = #treachery <> mapOneOf CardWithTrait [Power, Paradox]

instance RunMessage EstraviusMalone where
  runMessage msg e@(EstraviusMalone attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discarded <- getEncounterDiscard RegularEncounterDeck
      for_ (find (`cardMatch` topmostParadoxOrPower) discarded) \card -> do
        obtainCard card
        push $ InvestigatorDrewEncounterCard iid card
      pure e
    _ -> EstraviusMalone <$> liftRunMessage msg attrs
