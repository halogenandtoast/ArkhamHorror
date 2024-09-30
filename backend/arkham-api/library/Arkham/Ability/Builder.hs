module Arkham.Ability.Builder (
  module Arkham.Ability.Builder,
  module Arkham.Ability,
)
where

import Arkham.Ability
import Arkham.Ability.Types qualified
import Arkham.Action
import Arkham.Card.CardCode
import Arkham.Classes.Entity
import Arkham.Classes.HasAbilities
import Arkham.Helpers.Ability (withBaseAbilities)
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Source
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import GHC.Records

newtype AbilitiesBuilder e a = AbilitiesBuilder {runAbilitiesBuilder :: WriterT [Ability] (Reader e) a}
  deriving newtype (Functor, Applicative, Monad, MonadWriter [Ability], MonadReader e)

abilities :: e -> AbilitiesBuilder e () -> [Ability]
abilities e = flip runReader e . execWriterT . runAbilitiesBuilder

withActionAbility
  :: (HasCardCode e, Sourceable e) => Int -> ActionAbilityBuilder () -> AbilitiesBuilder e ()
withActionAbility idx body = do
  e <- ask
  AbilitiesBuilder $ tell [buildActionAbility e idx body]

withInvestigateAbility
  :: (HasCardCode e, Sourceable e) => Int -> ActionAbilityBuilder () -> AbilitiesBuilder e ()
withInvestigateAbility idx body = do
  e <- ask
  AbilitiesBuilder
    $ tell
      [ buildActionAbility
          e
          idx
          (addAction #investigate >> addCriteria (exists $ YourLocation <> InvestigatableLocation) >> body)
      ]

newtype ActionAbilityBuilder a = ActionAbilityBuilder {runAbilityBuilder :: Ability -> (a, Ability)}

instance Functor ActionAbilityBuilder where
  fmap :: (a -> b) -> ActionAbilityBuilder a -> ActionAbilityBuilder b
  fmap f (ActionAbilityBuilder g) = ActionAbilityBuilder $ \ability ->
    let (a, ability') = g (ability)
     in (f a, ability')

instance Applicative ActionAbilityBuilder where
  pure :: a -> ActionAbilityBuilder a
  pure x = ActionAbilityBuilder $ \ability -> (x, ability)

  (<*>) :: ActionAbilityBuilder (a -> b) -> ActionAbilityBuilder a -> ActionAbilityBuilder b
  (ActionAbilityBuilder f) <*> (ActionAbilityBuilder g) = ActionAbilityBuilder $ \ability ->
    let (fab, ability') = f ability
        (a, ability'') = g ability'
     in (fab a, ability'')

instance Monad ActionAbilityBuilder where
  (>>=) :: ActionAbilityBuilder a -> (a -> ActionAbilityBuilder b) -> ActionAbilityBuilder b
  (ActionAbilityBuilder g) >>= f = ActionAbilityBuilder $ \ability ->
    let (a, ability') = g ability
        ActionAbilityBuilder h = f a
     in h ability'

buildActionAbility
  :: (HasCardCode a, Sourceable a) => a -> Int -> ActionAbilityBuilder () -> Ability
buildActionAbility entity idx body = snd $ runAbilityBuilder body $ mkAbility entity idx $ ActionAbility [] (ActionCost 1)

addAction :: Action -> ActionAbilityBuilder ()
addAction a = ActionAbilityBuilder $ \ab ->
  let
    abilityType' =
      case abilityType ab of
        ActionAbility as c -> ActionAbility (as <> [a]) c
        ActionAbilityWithSkill as st c -> ActionAbilityWithSkill (as <> [a]) st c
        x -> x
   in
    ((), ab {Arkham.Ability.Types.abilityType = abilityType'})

addCost :: Cost -> ActionAbilityBuilder ()
addCost c = ActionAbilityBuilder $ \ab -> ((), overCost (<> c) ab)

justCost :: Cost -> ActionAbilityBuilder ()
justCost c = ActionAbilityBuilder $ \ab -> ((), overCost (const c) ab)

addCriteria :: Criterion -> ActionAbilityBuilder ()
addCriteria c = ActionAbilityBuilder $ \ab -> ((), ab {abilityCriteria = abilityCriteria ab <> c})

mustExist :: Exists a => a -> ActionAbilityBuilder ()
mustExist a = ActionAbilityBuilder $ \ab -> ((), ab {abilityCriteria = abilityCriteria ab <> exists a})

mustControl :: ActionAbilityBuilder ()
mustControl = addCriteria ControlsThis

extendRevealedAbilities
  :: (HasField "revealed" attrs Bool, attrs ~ EntityAttrs e, Entity e, HasAbilities attrs)
  => e
  -> AbilitiesBuilder e ()
  -> [Ability]
extendRevealedAbilities e action =
  withBaseAbilities (toAttrs e)
    $ guard ((toAttrs e).revealed)
    *> runReader (execWriterT $ runAbilitiesBuilder action) e

revealedSide
  :: (HasField "revealed" attrs Bool, attrs ~ EntityAttrs e, Entity e)
  => AbilitiesBuilder e ()
  -> AbilitiesBuilder e ()
revealedSide action = do
  e <- ask
  when ((toAttrs e).revealed) action
