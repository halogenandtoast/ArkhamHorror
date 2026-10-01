module Arkham.Homebrew.CircusExMortis.Enemies.ShubNiggurath (shubNiggurath) where

import Arkham.Ability
import Arkham.Card
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.Query (getLead, getSetAsideCardsMatching)
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (scenarioI18n)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype ShubNiggurath = ShubNiggurath EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | Massive and Retaliate are printed keywords and live on the card def.
shubNiggurath :: EnemyCard ShubNiggurath
shubNiggurath = enemy ShubNiggurath Cards.shubNiggurath

{- | It has no printed health at all: it can be fought and evaded, but never defeated, so
damage simply accumulates on it. "Shub-Niggurath cannot make attacks of opportunity or be
moved by player effects" -- the agenda's "move Shub-Niggurath once toward Silent Clearing"
is a scenario effect, so it is unaffected by the movement ban.
-}
instance HasModifiersFor ShubNiggurath where
  getModifiersFor (ShubNiggurath a) =
    modifySelf a [CannotBeDefeated, CannotMakeAttacksOfOpportunity, CannotBeMovedBy SourceIsPlayerCard]

instance HasAbilities ShubNiggurath where
  getAbilities (ShubNiggurath a) = extend1 a $ mkAbility a 1 $ forced $ RoundEnds #when

instance RunMessage ShubNiggurath where
  runMessage msg e@(ShubNiggurath attrs) = runQueueT $ case msg of
    {- "__Forced__ - At the end of the round: If there is no horror on Shub-Niggurath,
    place 1 horror on Shub-Niggurath. Otherwise, remove 1 horror from Shub-Niggurath and
    spawn a set-aside copy of Ravenous Brood at Shub-Niggurath's location. The
    investigators may choose which side to spawn face-up."

    The horror half always happens -- "do as much as you can" -- so with an empty set-aside
    pile the horror still comes off; what is withheld is the side prompt, which would
    otherwise ask the table to pick a face for a Brood that cannot arrive. -}
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      if attrs.token #horror == 0
        then placeTokens (attrs.ability 1) attrs #horror 1
        else do
          removeTokens (attrs.ability 1) attrs #horror 1
          broods <- getSetAsideCardsMatching (CardWithTitle "Ravenous Brood")
          for_ (take 1 broods) \brood -> withLocationOf attrs \lid -> do
            lead <- getLead
            -- the pile holds whichever face the copy was last set aside on
            let onHunterSide = exactCardCode brood == exactCardCode Cards.ravenousBrood_209
            let front = if onHunterSide then brood else flipCard brood
            chooseOneM lead $ scenarioI18n "thousandToOne" $ scope "ravenousBrood" do
              labeled "spawnHunterSide" $ spawnBroodAt front lid
              labeled "spawnAlertSide" $ spawnBroodAt (flipCard front) lid
      pure e
    _ -> ShubNiggurath <$> liftRunMessage msg attrs

{- | The chosen face has to be written back to the card map before the enemy is created,
or the engine builds the entity from whichever face happened to be set aside -- and the
set-aside entry is cleared by card id ('obtainCard') rather than by card equality, because
flipping changes the card code and so breaks @Eq Card@.
-}
spawnBroodAt :: ReverseQueue m => Card -> LocationId -> m ()
spawnBroodAt brood lid = do
  push $ ReplaceCard brood.id brood
  obtainCard brood
  createEnemyAt_ brood lid
