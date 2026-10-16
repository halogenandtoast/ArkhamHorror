module Arkham.Homebrew.TheLegendOfSleepyHollow.Treacheries.MarkedByTheHorseman (
  markedByTheHorseman,
) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), inThreatAreaGets)
import Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.TheLegendOfSleepyHollow.Helpers
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype MarkedByTheHorseman = MarkedByTheHorseman TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

markedByTheHorseman :: TreacheryCard MarkedByTheHorseman
markedByTheHorseman = treachery MarkedByTheHorseman Cards.markedByTheHorseman

{- | "You cannot activate [action] or [reaction] abilities on locations or
enemies." __Forced__ and basic abilities are exempt from
'CannotTriggerAbilityMatching', which is what the card means.
-}
instance HasModifiersFor MarkedByTheHorseman where
  getModifiersFor (MarkedByTheHorseman a) =
    inThreatAreaGets
      a
      [ CannotTriggerAbilityMatching
          $ oneOf [AbilityIsActionAbility, AbilityIsReactionAbility]
          <> oneOf [AbilityOnLocation Anywhere, AbilityOnEnemy AnyEnemy]
      ]

instance HasAbilities MarkedByTheHorseman where
  getAbilities (MarkedByTheHorseman a) =
    [ scenarioI18n
        $ withI18nTooltip "markedByTheHorseman.shake"
        $ skillTestAbility
        $ restricted a 1 (InThreatAreaOf You) actionAbility
    ]

instance RunMessage MarkedByTheHorseman where
  runMessage msg t@(MarkedByTheHorseman attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      alreadyThere <-
        selectAny $ treacheryInThreatAreaOf iid <> treacheryIs Cards.markedByTheHorseman
      if alreadyThere then gainSurge attrs else placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 3)
      pure t
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      -- "even if he is at another location or out of play"
      horseman <- selectOne (IncludeOutOfPlayEnemy headlessHorseman)
      for_ horseman \eid -> initiateEnemyAttack eid (attrs.ability 1) iid
      pure t
    SkillTestEnds _ _ (isAbilitySource attrs 1 -> True) -> do
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> MarkedByTheHorseman <$> liftRunMessage msg attrs
