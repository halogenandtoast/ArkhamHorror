module Arkham.Event.Events.DecoyTrap (decoyTrap) where

import Arkham.Ability
import Arkham.Evade.Types
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Investigator
import Arkham.Helpers.Location (getCanMoveTo)
import Arkham.Helpers.Window (enteringEnemy)
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Modifier

newtype DecoyTrap = DecoyTrap EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

decoyTrap :: EventCard DecoyTrap
decoyTrap = event DecoyTrap Cards.decoyTrap

instance HasAbilities DecoyTrap where
  getAbilities (DecoyTrap a) = case a.attachedTo.location of
    Just lid ->
      [ withEvadeCriteriaOverride (CriteriaOverride canEvadeAtAnyLocation)
          $ controlled_ a 1
          $ triggeredAction #evade (EnemyEnters #after (LocationWithId lid) AnyEnemy) (exhaust a)
      ]
    _ -> []

instance RunMessage DecoyTrap where
  runMessage msg e@(DecoyTrap attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      lid <- getJustLocation iid
      place attrs (AttachedToLocation lid)
      pure e
    UseCardAbility iid (isSource attrs -> True) 1 (enteringEnemy -> eid) _ -> do
      sid <- getRandom
      let
        using sk = skillLabeled sk do
          chooseEvadeEnemyEdit sid iid (attrs.ability 1) \ce ->
            ce
              { chooseEvadeEnemyMatcher = evadeOverride (EnemyWithId eid)
              , chooseEvadeOverride = True
              , chooseEvadeSkillType = sk
              }
      skillTestModifier sid (attrs.ability 1) iid (AnySkillValue 1)
      chooseOneM iid do
        using #intellect
        using #agility
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      for_ attrs.attachedTo.location \lid -> do
        iidLocation <- getJustLocation iid
        when (iidLocation /= lid) do
          whenM (getCanMoveTo iid (toSource attrs) lid) do
            chooseOneM iid do
              labeled "Discard Decoy Trap to move to its location" do
                toDiscardBy iid (toSource attrs) attrs
                moveTo attrs iid lid
              labeled "Do not move" nothing
      pure e
    _ -> DecoyTrap <$> liftRunMessage msg attrs
