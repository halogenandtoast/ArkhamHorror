{- | The runner behind a debug-authored custom investigator.
See "Arkham.Custom.Enemy".

An investigator's stats are not 'CardDef' fields, so they come from the def's
meta (@health@, @sanity@, @willpower@, @intellect@, @combat@, @agility@), which
is where the card builder puts them.
-}
module Arkham.Custom.Investigator (CustomInvestigator (..), customInvestigator) where

import Arkham.Card.CardDef (CardDef, toCardDef)
import Arkham.Card.CustomCard (customMeta, customMetaMaybe)
import Arkham.Custom.Ability (
  customAbilities,
  customModifiers,
  isCustomAbility,
  runCustomAbility,
  runCustomHandlers,
  runCustomSteps,
 )
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Investigator.Import.Lifted (elderSignValue)
import Arkham.Investigator.Runner
import Arkham.Matcher (ValueMatcher (AnyValue))
import Arkham.Message.Lifted (onSucceedByEffect, tokenSkillTestOption)
import Arkham.Prelude

newtype CustomInvestigator = CustomInvestigator InvestigatorAttrs
  deriving anyclass IsInvestigator
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

customInvestigator :: CardDef -> InvestigatorCard CustomInvestigator
customInvestigator def =
  investigator CustomInvestigator def
    $ Stats
      { health = customMeta "health" 5 def
      , sanity = customMeta "sanity" 5 def
      , willpower = customMeta "willpower" 3 def
      , intellect = customMeta "intellect" 3 def
      , combat = customMeta "combat" 3 def
      , agility = customMeta "agility" 3 def
      }

{- | The elder sign's modifier, from the def's meta.

@_elderSign@ is a 'GameCalculation', so a plain number is a flat modifier and
anything richer (a field on your location, a count of something) is expressed
the same way a printed card would. What the elder sign *does* beyond its
modifier is an @_handlers@ entry listening for the reveal.
-}
instance HasChaosTokenValue CustomInvestigator where
  getChaosTokenValue iid ElderSign (CustomInvestigator attrs) | attrs `is` iid = do
    pure $ case customMetaMaybe "_elderSign" (toCardDef attrs) of
      Just calculation -> elderSignValue calculation
      Nothing -> ChaosTokenValue ElderSign mempty
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance HasModifiersFor CustomInvestigator where
  getModifiersFor (CustomInvestigator a) = customModifiers a

instance HasAbilities CustomInvestigator where
  getAbilities (CustomInvestigator a) = customAbilities a

instance RunMessage CustomInvestigator where
  runMessage msg x@(CustomInvestigator attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) idx | isCustomAbility attrs idx -> do
      runCustomAbility attrs iid idx
      pure x
    ElderSignEffect iid | attrs `is` iid -> do
      -- What it does on being revealed, beyond its modifier.
      runCustomSteps attrs iid "_elderSignSteps"
      -- And what it offers if the test is then passed. Registered as an option
      -- on the skill test, labelled with the token, rather than resolved as a
      -- prompt of its own -- which is both how the game presents it and how a
      -- player expects to meet it.
      withSkillTest \sid ->
        onSucceedByEffect sid AnyValue (ElderSignEffectSource iid) sid do
          tokenSkillTestOption ElderSign do
            runCustomSteps attrs iid "_elderSignSuccessSteps"
      pure x
    _ -> do
      runCustomHandlers attrs msg
      CustomInvestigator <$> liftRunMessage msg attrs
