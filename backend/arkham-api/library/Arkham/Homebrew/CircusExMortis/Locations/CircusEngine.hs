module Arkham.Homebrew.CircusExMortis.Locations.CircusEngine (circusEngine) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Calculation
import Arkham.Constants
import Arkham.Fight
import Arkham.Helpers.Enemy (isActionTarget)
import Arkham.Helpers.Investigator (DamageFor (..), damageValueFor)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Helpers.SkillTest.Lifted (fight)
import Arkham.History
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted hiding (choose)
import Arkham.Location.Types (Field (..))
import Arkham.Token (Token (..))

newtype CircusEngine = CircusEngine LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

circusEngine :: LocationCard CircusEngine
circusEngine = location CircusEngine Cards.circusEngine 4 (Static 2)

instance HasModifiersFor CircusEngine where
  getModifiersFor (CircusEngine a) = modifySelfWhen a a.revealed [CanBeAttackedAsIfEnemy]

instance HasAbilities CircusEngine where
  getAbilities (CircusEngine a) =
    extendRevealed
      a
      [ restricted a 1 Here $ actionAbilityWithCost $ AtLeastOne (Fixed 3) (ClueCost $ Static 1)
      , basicAbility $ restricted a AbilityAttack Here $ ActionAbility #fight #combat (ActionCost 1)
      ]

instance RunMessage CircusEngine where
  runMessage msg l@(CircusEngine attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 _ (totalCluePayment -> n) -> do
      placeTokens (attrs.ability 1) attrs Damage n
      pure l
    UseCardAbility iid (isSource attrs -> True) AbilityAttack _ _ -> do
      sid <- getRandom
      push $ FightEnemy (coerce attrs.id) $ mkChooseFightPure sid iid (attrs.ability AbilityAttack)
      pure l
    AttackEnemy eid choose | coerce eid == attrs.id -> do
      let target = maybe (toTarget attrs) (ProxyTarget (toTarget attrs)) choose.target
      let difficulty = case choose.difficulty of
            DefaultChooseFightDifficulty -> LocationMaybeFieldCalculation attrs.id LocationShroud
            CalculatedChooseFightDifficulty c -> c
      fight choose.skillTest choose.investigator choose.source target choose.skillType difficulty
      pure l
    PassedSkillTest iid (Just Action.Fight) source (Initiator target) _ n | isActionTarget attrs target -> do
      updateHistory iid (HistoryItem HistorySuccessfulAttacks 1)
      push $ Successful (Action.Fight, toProxyTarget target) iid source (toActionTarget target) n
      pure l
    Successful (Action.Fight, _) iid _ target _ | isTarget attrs target -> do
      dmg <- damageValueFor 1 iid DamageForEnemy
      placeTokens (attrs.ability AbilityAttack) attrs Damage (if dmg >= 3 then 2 else 1)
      pure l
    _ -> CircusEngine <$> liftRunMessage msg attrs
