{-# LANGUAGE ImplicitParams #-}

module Arkham.Ability.Scripted.Builder (
  module Arkham.Ability.Scripted.Builder,
  module Arkham.Ability.Scripted,
)
where

import Arkham.Ability qualified as A
import Arkham.Ability.Scripted hiding (fight, fightAction, forced, reaction)
import Arkham.Ability.Scripted qualified as S
import Arkham.Ability.Types qualified
import Arkham.Action
import Arkham.Card.CardCode
import Arkham.Classes.Entity
import Arkham.Id
import Arkham.Matcher
import Arkham.Message (Message)
import Arkham.Prelude
import Arkham.Source
import Arkham.Token
import Arkham.Window (Window)
import Control.Monad.Reader
import Control.Monad.State.Strict
import GHC.Records

-- import Arkham.Classes.HasAbilities
-- import Arkham.Helpers.Ability (withBaseAbilities)

newtype AbilitiesBuilder e a = AbilitiesBuilder {runAbilitiesBuilder :: StateT [ScriptedAbility e] (Reader e) a}
  deriving newtype (Functor, Applicative, Monad, MonadState [ScriptedAbility e], MonadReader e)

abilities :: ((?this :: e) => AbilitiesBuilder e ()) -> e -> [ScriptedAbility e]
abilities body e = let ?this = e in runReader (execStateT (runAbilitiesBuilder body) []) e

withActionAbility
  :: (HasCardCode e, Sourceable e) => Int -> ActionAbilityBuilder e () -> AbilitiesBuilder e ()
withActionAbility idx body = do
  e <- ask
  AbilitiesBuilder $ tell [buildActionAbility e idx body]

withInvestigateAbility
  :: (HasCardCode e, Sourceable e) => Int -> ActionAbilityBuilder e () -> AbilitiesBuilder e ()
withInvestigateAbility idx body = do
  e <- ask
  AbilitiesBuilder
    $ tell
      [ buildActionAbility
          e
          idx
          (addAction #investigate >> addCriteria (exists $ YourLocation <> InvestigatableLocation) >> body)
      ]

newtype ActionAbilityBuilder e a = ActionAbilityBuilder {runAbilityBuilder :: ScriptedAbility e -> (a, ScriptedAbility e)}

instance Functor (ActionAbilityBuilder e) where
  fmap :: (a -> b) -> ActionAbilityBuilder e a -> ActionAbilityBuilder e b
  fmap f (ActionAbilityBuilder g) = ActionAbilityBuilder $ \ab ->
    let (a, ability') = g ab
     in (f a, ability')

instance Applicative (ActionAbilityBuilder e) where
  pure :: a -> ActionAbilityBuilder e a
  pure x = ActionAbilityBuilder $ \ab -> (x, ab)

  (<*>) :: ActionAbilityBuilder e (a -> b) -> ActionAbilityBuilder e a -> ActionAbilityBuilder e b
  (ActionAbilityBuilder f) <*> (ActionAbilityBuilder g) = ActionAbilityBuilder $ \ab ->
    let (fab, ability') = f ab
        (a, ability'') = g ability'
     in (fab a, ability'')

instance Monad (ActionAbilityBuilder e) where
  (>>=) :: ActionAbilityBuilder e a -> (a -> ActionAbilityBuilder e b) -> ActionAbilityBuilder e b
  (ActionAbilityBuilder g) >>= f = ActionAbilityBuilder $ \ab ->
    let (a, ability') = g ab
        ActionAbilityBuilder h = f a
     in h ability'

buildActionAbility
  :: (HasCardCode a, Sourceable a) => a -> Int -> ActionAbilityBuilder a () -> ScriptedAbility a
buildActionAbility entity idx body =
  snd
    $ runAbilityBuilder body
    $ ScriptedAbility (mkAbility entity idx $ ActionAbility [] (ActionCost 1)) (AbilityScript $ pure ())

addAction :: Action -> ActionAbilityBuilder a ()
addAction a = ActionAbilityBuilder $ \(ScriptedAbility ab s) ->
  let
    abilityType' =
      case abilityType ab of
        ActionAbility as c -> ActionAbility (as <> [a]) c
        ActionAbilityWithSkill as st c -> ActionAbilityWithSkill (as <> [a]) st c
        x -> x
   in
    ((), ScriptedAbility (ab {Arkham.Ability.Types.abilityType = abilityType'}) s)

tooltip :: Text -> ActionAbilityBuilder a ()
tooltip t = ActionAbilityBuilder $ \(ScriptedAbility ab s) -> ((), ScriptedAbility (withTooltip t ab) s)

addCost :: Cost -> ActionAbilityBuilder a ()
addCost c = ActionAbilityBuilder $ \(ScriptedAbility ab s) -> ((), ScriptedAbility (overCost (<> c) ab) s)

spendUses
  :: (?this :: a, Entity a, EntityId a ~ AssetId) => Int -> Token -> ActionAbilityBuilder a ()
spendUses n c = ActionAbilityBuilder $ \(ScriptedAbility ab s) -> ((), ScriptedAbility (overCost (<> assetUseCost ?this c n) ab) s)

justCost :: Cost -> ActionAbilityBuilder a ()
justCost c = ActionAbilityBuilder $ \(ScriptedAbility ab s) -> ((), ScriptedAbility (overCost (const c) ab) s)

addCriteria :: Criterion -> ActionAbilityBuilder a ()
addCriteria c = ActionAbilityBuilder $ \(ScriptedAbility ab s) -> ((), ScriptedAbility (ab {abilityCriteria = abilityCriteria ab <> c}) s)

mustExist :: Exists a => a -> ActionAbilityBuilder a ()
mustExist a = ActionAbilityBuilder $ \(ScriptedAbility ab s) -> ((), ScriptedAbility (ab {abilityCriteria = abilityCriteria ab <> exists a}) s)

mustControl :: ActionAbilityBuilder a ()
mustControl = addCriteria ControlsThis

inYourHand :: ActionAbilityBuilder a ()
inYourHand = addCriteria InYourHand

self :: ActionAbilityBuilder a ()
self = addCriteria Self

oncePerRound :: ActionAbilityBuilder a ()
oncePerRound = overAbility (playerLimit PerRound)

overAbility :: (Ability -> Ability) -> ActionAbilityBuilder a ()
overAbility f = ActionAbilityBuilder $ \(ScriptedAbility ab s) -> ((), ScriptedAbility (f ab) s)

run
  :: ((?you :: InvestigatorId, ?this :: a, ?source :: Source) => ScriptT a ())
  -> ActionAbilityBuilder a ()
run s = ActionAbilityBuilder \(ScriptedAbility ab (AbilityScript s')) -> ((), ScriptedAbility ab (AbilityScript $ s' >> s))

elderSign
  :: (?this :: a, Sourceable a, HasCardCode a)
  => ( ( ?this :: a
       , ?msg :: Message
       , ?you :: InvestigatorId
       , ?ability :: Source
       , ?source :: Source
       , ?windows :: [Window]
       )
       => ScriptT a ()
     )
  -> AbilitiesBuilder a ()
elderSign body = do
  idx <- abilitiesCount
  AbilitiesBuilder do
    tell [ScriptedAbility (mkAbility ?this idx ConstantAbility) (AbilityScript body)]

extendRevealedAbilities
  :: (HasField "revealed" attrs Bool, attrs ~ EntityAttrs e, Entity e)
  => e
  -> AbilitiesBuilder e ()
  -> [ScriptedAbility e]
extendRevealedAbilities e action =
  guard ((toAttrs e).revealed)
    *> runReader (execStateT (runAbilitiesBuilder action) []) e

revealedSide
  :: (HasField "revealed" attrs Bool, attrs ~ EntityAttrs e, Entity e)
  => AbilitiesBuilder e ()
  -> AbilitiesBuilder e ()
revealedSide action = do
  e <- ask
  when ((toAttrs e).revealed) action

tell :: [ScriptedAbility e] -> StateT [ScriptedAbility e] (Reader e) ()
tell xs = modify (<> xs)

forced
  :: (HasCardCode e, Sourceable e) => WindowMatcher -> ActionAbilityBuilder e () -> AbilitiesBuilder e ()
forced matcher body = do
  e <- ask
  idx <- abilitiesCount
  AbilitiesBuilder do
    tell [buildForcedAbility e (idx + 1) matcher body]

reaction
  :: (HasCardCode e, Sourceable e) => WindowMatcher -> ActionAbilityBuilder e () -> AbilitiesBuilder e ()
reaction matcher body = do
  e <- ask
  idx <- abilitiesCount
  AbilitiesBuilder do
    tell [buildReactionAbility e (idx + 1) matcher body]

buildReactionAbility
  :: (HasCardCode a, Sourceable a)
  => a -> Int -> WindowMatcher -> ActionAbilityBuilder a () -> ScriptedAbility a
buildReactionAbility entity idx matcher body =
  snd
    $ runAbilityBuilder body
    $ ScriptedAbility (mkAbility entity idx $ A.freeReaction matcher) (AbilityScript $ pure ())

buildForcedAbility
  :: (HasCardCode a, Sourceable a)
  => a -> Int -> WindowMatcher -> ActionAbilityBuilder a () -> ScriptedAbility a
buildForcedAbility entity idx matcher body =
  snd
    $ runAbilityBuilder body
    $ ScriptedAbility (mkAbility entity idx $ A.forced matcher) (AbilityScript $ pure ())

abilitiesCount :: AbilitiesBuilder e Int
abilitiesCount = length <$> get

fightAction
  :: (HasCardCode e, Sourceable e) => ActionAbilityBuilder e () -> AbilitiesBuilder e ()
fightAction body = do
  e <- ask
  idx <- abilitiesCount
  AbilitiesBuilder do
    tell [buildActionAbility e (idx + 1) (mustControl >> addAction #fight >> let ?this = e in body)]

fight
  :: ((?you :: InvestigatorId, ?source :: Source) => FightT (ScriptT a) ()) -> ActionAbilityBuilder a ()
fight body = run $ S.fight body
