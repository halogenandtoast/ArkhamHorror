module Arkham.Homebrew.DarkMatter.Treacheries.Reminiscence (
  reminiscenceCovenant,
  reminiscencePledge,
  reminiscenceSecrets,
) where

import Arkham.Ability
import Arkham.Card
import Arkham.Enemy.Types (Field (EnemyDamage))
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Projection
import Arkham.Treachery.Import.Lifted hiding (EnemyEvaded, InvestigatorEliminated)

{- | The three Reminiscence [[Pact]] weaknesses share a shell:

"Hidden. Peril. / Revelation - Secretly add this card to your hand. /
Forced - When the game ends, or if you are eliminated, if this card is in your
hand: Add it to the victory display."

and differ in the reaction that lets you be rid of them:

* Pledge — "When an investigator would defeat an enemy at your location, heal all
  damage from it instead."
* Secrets — "When an investigator would discover any amount of clues from your
  location, place 1 of their clues on that location instead."
* Covenant — "When an investigator would successfully evade an enemy at your
  location, that enemy immediately attacks them instead."
-}
newtype Reminiscence = Reminiscence TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mkReminiscence :: CardDef -> TreacheryCard Reminiscence
mkReminiscence = treachery Reminiscence

reminiscencePledge :: TreacheryCard Reminiscence
reminiscencePledge = mkReminiscence Cards.reminiscencePledge

reminiscenceSecrets :: TreacheryCard Reminiscence
reminiscenceSecrets = mkReminiscence Cards.reminiscenceSecrets

reminiscenceCovenant :: TreacheryCard Reminiscence
reminiscenceCovenant = mkReminiscence Cards.reminiscenceCovenant

reminiscenceIs :: TreacheryAttrs -> CardDef -> Bool
reminiscenceIs a def = toCardCode (toCardDef a) == toCardCode def

instance HasAbilities Reminiscence where
  getAbilities (Reminiscence a) =
    [ mkAbility a 1 $ forced $ oneOf [GameEnds #when, InvestigatorEliminated #when You]
    , mkAbility a 2 $ freeReaction $ escapeWindow a
    ]
   where
    here = maybe Anywhere (LocationWithInvestigator . InvestigatorWithId) a.inThreatAreaOf
    escapeWindow attrs
      | reminiscenceIs attrs Cards.reminiscenceSecrets = WouldDiscoverClues #when Anyone here AnyValue
      | reminiscenceIs attrs Cards.reminiscenceCovenant = EnemyEvaded #after Anyone (EnemyAt here)
      | otherwise = EnemyWouldBeDefeated #when (EnemyAt here)

instance RunMessage Reminiscence where
  runMessage msg t@(Reminiscence attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      addHiddenToHand iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      addToVictory iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let here = maybe Anywhere (LocationWithInvestigator . InvestigatorWithId) attrs.inThreatAreaOf
      if
        | reminiscenceIs attrs Cards.reminiscenceSecrets ->
            -- "place 1 of their clues on that location instead of discovering clues"
            selectOne here >>= traverse_ \lid -> moveTokens (attrs.ability 2) iid lid #clue 1
        | reminiscenceIs attrs Cards.reminiscenceCovenant ->
            -- "that enemy immediately attacks them instead"
            selectOne (EnemyAt here) >>= traverse_ \eid -> initiateEnemyAttack eid (attrs.ability 2) iid
        | otherwise ->
            -- "heal all damage from it instead"
            selectOne (EnemyAt here <> EnemyWithDamage (atLeast 1)) >>= traverse_ \eid -> do
              damage <- field EnemyDamage eid
              healDamage eid (attrs.ability 2) damage
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> Reminiscence <$> liftRunMessage msg attrs
