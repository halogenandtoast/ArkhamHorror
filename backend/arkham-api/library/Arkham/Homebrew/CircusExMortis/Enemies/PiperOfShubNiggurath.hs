module Arkham.Homebrew.CircusExMortis.Enemies.PiperOfShubNiggurath (piperOfShubNiggurath) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types (Field (EnemyDamage))
import Arkham.Helpers.Enemy (disengageEnemyFromAll)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.CircusExMortis.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.CircusExMortis.Helpers (
  flipToVictoryDisplay,
  investigatorWithDestinyModifier,
 )
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (enemyMoveTo)
import Arkham.SkillType (allSkills)

newtype PiperOfShubNiggurath = PiperOfShubNiggurath EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

piperOfShubNiggurath :: EnemyCard PiperOfShubNiggurath
piperOfShubNiggurath = enemy PiperOfShubNiggurath Cards.piperOfShubNiggurath

-- | "Piper of Shub-Niggurath cannot be defeated." Its 12 health only feeds the parley.
instance HasModifiersFor PiperOfShubNiggurath where
  getModifiersFor (PiperOfShubNiggurath a) = modifySelf a [CannotBeDefeated]

{- | Both abilities name a destiny, and 'getAbilities' is pure, so they go through the
matcher form backed by the 'ScenarioModifier's Thousand to One republishes -- not the
log-reading 'investigatorWithDestiny', which only works inside a 'HasModifiersFor'.

Ability 2 is gated on the acting investigator holding the "pipes" destiny so the parley is
never offered to a seat that cannot win it; if nobody at the table drew "pipes", the
matcher is vacuous and the action never appears.
-}
instance HasAbilities PiperOfShubNiggurath where
  getAbilities (PiperOfShubNiggurath a) =
    extend
      a
      [ restricted
          a
          1
          (oneOf [exists $ connectedTo (locationWithEnemy a), thisExists a (EnemyIsEngagedWith Anyone)])
          $ forced
          $ EnemyTakeDamage #after AnyDamageEffect (be a) (atLeast 1)
          $ SourceUsedBy
          $ NotInvestigator
          $ investigatorWithDestinyModifier "pipes"
      , skillTestAbility
          $ restricted
            a
            2
            (OnSameLocation <> youExist (investigatorWithDestinyModifier "pipes"))
            parleyAction_
      ]

instance RunMessage PiperOfShubNiggurath where
  runMessage msg e@(PiperOfShubNiggurath attrs) = runQueueT $ case msg of
    -- "Forced - After Piper of Shub-Niggurath takes 1 or more damage from an investigator
    -- whose destiny is not 'pipes': It disengages and moves to a connecting location."
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      disengageEnemyFromAll attrs
      withLocationOf attrs \lid -> do
        locations <- select $ connectedTo (LocationWithId lid) <> LocationCanBeEnteredBy attrs.id
        lead <- getLead
        chooseOrRunOneM lead $ targets locations $ enemyMoveTo (attrs.ability 1) attrs
      pure e
    -- "[action] If your destiny is 'pipes': Parley. You challenge the piper in a game of
    -- skill. Test any skill (12). This test gets -1 difficulty for each damage on Piper of
    -- Shub-Niggurath." The difficulty stays a calculation so it is read when the test
    -- resolves, not when the action is taken.
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      chooseOneM iid do
        for_ allSkills \sType ->
          skillLabeled sType
            $ parley sid iid (attrs.ability 2) attrs sType
            $ SubtractCalculation (Fixed 12) (EnemyFieldCalculation attrs.id EnemyDamage)
      pure e
    -- "If you succeed, flip it and move it to the victory display."
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      flipToVictoryDisplay (Just iid) Stories.silenceThePipes attrs.cardId $ RemoveEnemy attrs.id
      pure e
    _ -> PiperOfShubNiggurath <$> liftRunMessage msg attrs
