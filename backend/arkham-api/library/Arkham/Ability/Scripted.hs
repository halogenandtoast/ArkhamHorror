{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RoleAnnotations #-}

module Arkham.Ability.Scripted (module Arkham.Ability.Scripted, module X) where

import Arkham.Ability as X hiding (atYourLocation, you)
import Arkham.Classes.Entity
import Arkham.Classes.RunMessage
import Arkham.Classes.HasAbilities
import Arkham.GameT
import Arkham.Id
import Arkham.Message
import Arkham.Investigator.Types
import Data.Typeable
import Arkham.Prelude
import Arkham.Script as X
import Arkham.Source
import Arkham.Window (Window)
import GHC.Records

type role Scripted representational
newtype Scripted a = Scripted a

instance ScriptedAbilities a => HasAbilities (Scripted a) where
  getAbilities (Scripted a) = useScripted a

instance (ScriptedAbilities a, attrs ~ EntityAttrs a, RunType attrs ~ attrs, Sourceable attrs, RunMessage attrs, HasField "ability" attrs (Int -> Source), Typeable attrs) => RunMessage (Scripted a) where
  type RunType (Scripted a) = a
  runMessage msg (Scripted a) = withScripted (pure ()) msg a

newtype AbilityScript a
  = AbilityScript
      ( ( ?this :: a
        , ?msg :: Message
        , ?you :: InvestigatorId
        , ?ability :: Source
        , ?source :: Source
        , ?windows :: [Window]
        )
        => ScriptT a ()
      )

data ScriptedAbility a = ScriptedAbility Ability (AbilityScript a)

useScripted :: ScriptedAbilities a => a -> [Ability]
useScripted = map scriptedAbility . scriptedAbilities

scriptedAbility :: ScriptedAbility a -> Ability
scriptedAbility (ScriptedAbility ab _) = ab

class Entity a => ScriptedAbilities a where
  scriptedAbilities :: a -> [ScriptedAbility a]

withScripted
  :: forall a attrs. ( ScriptedAbilities a
     , attrs ~ EntityAttrs a
     , Sourceable attrs
     , RunMessage attrs
     , HasField "ability" attrs (Int -> Source)
     , RunType attrs ~ attrs
     , Typeable attrs
     )
  => ((?this :: a) => ScriptT a ())
  -> Message
  -> a
  -> GameT a
withScripted body = script do
  for_ (scriptedAbilities ?this) \(ScriptedAbility ab (AbilityScript handler)) ->
    if ab.kind == ConstantAbility
      then case eqT @attrs @InvestigatorAttrs of
        Just Refl -> let ?ability = toSource ab in elderSignEffect handler
        Nothing -> pure ()
      else if isInHand ab
        then inHand $ onAbility ab.index handler
        else onAbility ab.index handler
  body
 where
  isInHand ab = go ab.criteria
  go InYourHand = True
  go (Criteria xs) = any go xs
  go _ = False

scripted
  :: Ability
  -> ( ( ?this :: a
       , ?msg :: Message
       , ?you :: InvestigatorId
       , ?ability :: Source
       , ?source :: Source
       , ?windows :: [Window]
       )
       => ScriptT a ()
     )
  -> ScriptedAbility a
scripted ab body = ScriptedAbility ab $ AbilityScript body

