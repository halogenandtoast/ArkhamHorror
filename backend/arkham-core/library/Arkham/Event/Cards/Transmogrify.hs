module Arkham.Event.Cards.Transmogrify (transmogrify, Transmogrify (..)) where

import Arkham.Ability
import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted hiding (EnemyEvaded)
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified)
import Arkham.Helpers.SkillTest.Target
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Placement
import Arkham.Projection

newtype Transmogrify = Transmogrify EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

transmogrify :: EventCard Transmogrify
transmogrify = event Transmogrify Cards.transmogrify

instance HasModifiersFor Transmogrify where
  getModifiersFor (EnemyTarget eid) (Transmogrify a) = maybeModified a do
    EnemyTarget eid' <- hoistMaybe $ a.placement.attachedTo
    guard $ eid == eid'
    pure [AddKeyword Keyword.Massive, CannotMove]
  getModifiersFor _ _ = pure []

instance HasAbilities Transmogrify where
  getAbilities (Transmogrify x) = case x.placement of
    AttachedToEnemy eid ->
      [ withTooltip "Discover 1 clue at its location (Transmogrify)"
          $ playerLimit PerRound
          $ restrictedAbility
            (proxied eid x)
            1
            (exists $ locationWithEnemy eid <> LocationWithDiscoverableCluesBy You)
          $ freeReaction
          $ EnemyEvaded #after You (EnemyWithId eid)
      ]
    _ -> []

instance RunMessage Transmogrify where
  runMessage msg e@(Transmogrify attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      chooseEvadeEnemyWithSkillChoice sid iid attrs [#intellect, #agility]
      pure e
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      getSkillTestTarget >>= \case
        Just (EnemyTarget eid) -> do
          whenM (eid <=~> NonEliteEnemy) do
            push $ PlaceEvent iid attrs.id $ AttachedToEnemy eid
        _ -> pure ()
      pure e
    UseThisAbility iid (isProxySource attrs -> True) 1 -> do
      case attrs.placement of
        AttachedToEnemy eid -> do
          whenJustM (field EnemyLocation eid) \lid -> do
            discoverAt NotInvestigate iid (attrs.ability 1) lid 1
        _ -> error "Invalid placement"
      pure e
    _ -> Transmogrify <$> liftRunMessage msg attrs
