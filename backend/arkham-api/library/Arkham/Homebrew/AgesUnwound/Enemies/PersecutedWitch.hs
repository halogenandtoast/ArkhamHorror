module Arkham.Homebrew.AgesUnwound.Enemies.PersecutedWitch (persecutedWitch) where

import Arkham.Card
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Doom (getDoomCount)
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Hex, Paradox))

newtype PersecutedWitch = PersecutedWitch EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

persecutedWitch :: EnemyCard PersecutedWitch
persecutedWitch = enemy PersecutedWitch Cards.persecutedWitch

{- | "Revelation - For each doom in play (min 1), discard the top card of the
encounter deck. Draw a [[Hex]] or [[Paradox]] treachery discarded this way."

An enemy with a Revelation: the def carries @cdRevelation = IsRevelation@, so the
engine spawns the enemy and then runs the Revelation as two separate steps.
-}
instance RunMessage PersecutedWitch where
  runMessage msg e@(PersecutedWitch attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      doom <- getDoomCount
      discardTopOfEncounterDeckAndHandle iid attrs (max 1 doom) attrs
      pure e
    DiscardedTopOfEncounterDeck iid cards (isSource attrs -> True) (isTarget attrs -> True) -> do
      -- "discarded this way": only the cards this Revelation turned over.
      let candidates =
            filterCards (CardWithType TreacheryType <> mapOneOf CardWithTrait [Hex, Paradox]) cards
      unless (null candidates) do
        focusCards cards $ chooseOneM iid $ targets candidates (drawCard iid)
      pure e
    _ -> PersecutedWitch <$> liftRunMessage msg attrs
