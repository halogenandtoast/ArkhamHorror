module Arkham.Treachery.Cards.UnspeakableOathCowardice (
  unspeakableOathCowardice,
  UnspeakableOathCowardice (..),
)
where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Evade
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Matcher hiding (EnemyEvaded, TreacheryInHandOf)
import Arkham.Matcher qualified as Matcher
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype UnspeakableOathCowardice = UnspeakableOathCowardice TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unspeakableOathCowardice :: TreacheryCard UnspeakableOathCowardice
unspeakableOathCowardice = treachery UnspeakableOathCowardice Cards.unspeakableOathCowardice

evasionCriteria :: ModifierType
evasionCriteria =
  EnemyEvadeActionCriteria
    $ CriteriaOverride
    $ EnemyCriteria
    $ ThisEnemy
    $ ExhaustedEnemy
    <> UnengagedEnemy
    <> EnemyAt YourLocation

instance HasModifiersFor UnspeakableOathCowardice where
  getModifiersFor (UnspeakableOathCowardice a) = do
    case a.placement of
      HiddenInHand iid -> do
        selectOne (AbilityIs (toSource a) 2) >>= \case
          Nothing -> pure mempty
          Just ab -> modified_ a (AbilityTarget iid ab) [CanModify evasionCriteria]
      _ -> pure mempty

instance HasAbilities UnspeakableOathCowardice where
  getAbilities (UnspeakableOathCowardice attrs) =
    [ restrictedAbility attrs 1 InYourHand
        $ forced
        $ oneOf [Matcher.GameEnds #when, Matcher.InvestigatorEliminated #when You]
    , evadeAbility attrs 2 (ActionCost 1) InYourHand
    ]

instance RunMessage UnspeakableOathCowardice where
  runMessage msg t@(UnspeakableOathCowardice attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeTreachery attrs (HiddenInHand iid)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gameModifier attrs iid (XPModifier "Unspeakable Oath: Cowardice" (-2))
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let source = toAbilitySource attrs 2
      let matcher = ExhaustedEnemy <> UnengagedEnemy
      sid <- getRandom
      skillTestModifier sid (toAbilitySource attrs 2) iid evasionCriteria
      pushM $ toMessage . setTarget attrs <$> mkChooseEvadeMatch sid iid source matcher
      pure t
    Successful (Action.Evade, EnemyTarget eid) iid _ (isTarget attrs -> True) _ -> do
      push $ EnemyEvaded iid eid
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> UnspeakableOathCowardice <$> liftRunMessage msg attrs
