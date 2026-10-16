module Arkham.Homebrew.AndThenThereWereNone.Enemies.RestlessSpirit_040 (
  restlessSpirit_040,
) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted hiding (RevealChaosToken)
import Arkham.Helpers.Window (getChaosToken)
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.AndThenThereWereNone.Helpers
import Arkham.Matcher

newtype RestlessSpirit_040 = RestlessSpirit_040 EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | "Spawn - Anthony Marston's or Emily Brent's location."
restlessSpirit_040 :: EnemyCard RestlessSpirit_040
restlessSpirit_040 =
  enemy RestlessSpirit_040 Cards.restlessSpirit_040
    & setSpawnAt (guestsLocation [Assets.anthonyMarston, Assets.emilyBrent])

{- | "__Forced__ - When you reveal a {cultist}, {tablet}, or {elderThing} token
at Restless Spirit's location: Resolve the additional token effects of all three
tokens (but not their modifiers)."
-}
instance HasAbilities RestlessSpirit_040 where
  getAbilities (RestlessSpirit_040 a) =
    extend1 a
      $ scenarioI18n
      $ withI18nTooltip "restlessSpirit_040.tokens"
      $ restricted a 1 (youExist $ at_ $ locationWithEnemy a)
      $ forced
      $ RevealChaosToken #when You (oneOf [#cultist, #tablet, #elderthing])

{- | 'ResolveChaosToken' is the effects half of a token; the numeric modifier
comes from 'getChaosTokenValue' separately, so resolving the other faces here
takes their effects and leaves their values alone -- "but not their modifiers".

Only the two faces that were NOT revealed are resolved. The revealed one
resolves on its own at ST.6, so the three effects land once each; firing all
three from this {when} window would double the one in hand.
-}
instance RunMessage RestlessSpirit_040 where
  runMessage msg e@(RestlessSpirit_040 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getChaosToken -> token) _ -> do
      for_ (filter (/= token.face) [#cultist, #tablet, #elderthing]) \face ->
        push $ ResolveChaosToken token face iid
      pure e
    _ -> RestlessSpirit_040 <$> liftRunMessage msg attrs
