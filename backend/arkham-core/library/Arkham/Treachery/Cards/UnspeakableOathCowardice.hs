module Arkham.Treachery.Cards.UnspeakableOathCowardice (
  unspeakableOathCowardice,
  UnspeakableOathCowardice (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Matcher hiding (EnemyEvaded, TreacheryInHandOf)
import Arkham.Matcher qualified as Matcher
import Arkham.Message
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype UnspeakableOathCowardice = UnspeakableOathCowardice TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unspeakableOathCowardice :: TreacheryCard UnspeakableOathCowardice
unspeakableOathCowardice = treachery UnspeakableOathCowardice Cards.unspeakableOathCowardice

evasionCriteria :: ModifierType
evasionCriteria =
  EnemyEvadeActionCriteria
    $ CriteriaOverride
    $ EnemyCriteria
    $ ThisEnemy
    $ ExhaustedEnemy <> UnengagedEnemy <> EnemyAt YourLocation

instance HasModifiersFor UnspeakableOathCowardice where
  getModifiersFor (AbilityTarget iid ability) (UnspeakableOathCowardice a)
    | treacheryOwner a == Just iid
    , abilitySource ability == toSource a
    , abilityIndex ability == 2 =
        pure $ toModifiers a [CanModify evasionCriteria]
  getModifiersFor _ _ = pure []

instance HasAbilities UnspeakableOathCowardice where
  getAbilities (UnspeakableOathCowardice attrs) =
    [ restrictedAbility attrs 1 InYourHand
        $ ForcedAbility
        $ OrWindowMatcher
          [ Matcher.GameEnds #when
          , Matcher.InvestigatorEliminated #when You
          ]
    , evadeAbility attrs 2 (ActionCost 1) InYourHand
    ]

instance RunMessage UnspeakableOathCowardice where
  runMessage msg t@(UnspeakableOathCowardice attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ PlaceTreachery (toId attrs) (TreacheryInHandOf iid)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ gameModifier attrs iid (XPModifier (-2))
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let source = toAbilitySource attrs 2
      let matcher = ExhaustedEnemy <> UnengagedEnemy
      pushAll
        [ skillTestModifier (toAbilitySource attrs 2) iid evasionCriteria
        , ChooseEvadeEnemy iid source (Just $ toTarget attrs) #agility matcher False
        ]
      pure t
    Successful (Action.Evade, EnemyTarget eid) iid _ (isTarget attrs -> True) _ -> do
      pushAll [EnemyEvaded iid eid, Discard (toAbilitySource attrs 2) $ toTarget attrs]
      pure t
    _ -> UnspeakableOathCowardice <$> runMessage msg attrs
