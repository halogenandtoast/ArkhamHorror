{- | Terrace -- the [[Central]] hub, and the only way off the island.

The hunter action reads the keyword on an enemy that is somewhere ELSE, so "if
an enemy entered Terrace" is the same question as "is the enemy you chose here
now": it was at another location when it was chosen, and the hunter move is the
only movement the action causes.
-}
module Arkham.Homebrew.AndThenThereWereNone.Locations.Terrace (terrace) where

import Arkham.Ability
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AndThenThereWereNone.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype Terrace = Terrace LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

terrace :: LocationCard Terrace
terrace = location Terrace Cards.terrace 1 (Static 0)

{- | "an enemy at any other location"

Engaged enemies are left out: the hunter keyword only ever moves an unengaged
enemy, and 'HunterMove' itself does not check engagement.
-}
enemyElsewhere :: LocationAttrs -> EnemyMatcher
enemyElsewhere a = UnengagedEnemy <> EnemyAt (not_ $ be a)

instance HasAbilities Terrace where
  getAbilities (Terrace a) =
    extendRevealed
      a
      [ scenarioI18n
          $ withI18nTooltip "terrace.hunter"
          $ restricted a 1 (Here <> exists (enemyElsewhere a)) actionAbility
      , scenarioI18n
          $ withI18nTooltip "terrace.resign"
          -- "If each undefeated investigator is at Terrace"
          $ restricted
            a
            2
            (Here <> notExists (UneliminatedInvestigator <> not_ (investigatorAt a.id)))
            resignAction_
      ]

instance RunMessage Terrace where
  runMessage msg l@(Terrace attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ enemyElsewhere attrs
      chooseTargetM iid enemies \enemy -> do
        resolveHunterKeyword enemy
        handleTarget iid (attrs.ability 1) enemy
      pure l
    -- "Then, if an enemy entered Terrace, gain 1 clue from the token bank."
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (EnemyTarget enemy) -> do
      whenM (enemy <=~> enemyAt attrs.id) $ gainClues iid (attrs.ability 1) 1
      pure l
    -- "Each investigator resigns."
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      selectEach UneliminatedInvestigator $ push . Resign
      pure l
    _ -> Terrace <$> liftRunMessage msg attrs
